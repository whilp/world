const GROUP_CACHE_TTL = 21600; // 6 hours
const MAX_RUNTIME_MS = 180000; // 3 minutes

// Check if email is a group (not a person) using People API
function isGroup(email) {
  if (email.includes('@resource.calendar.google.com')) {
    return false;
  }

  const cache = CacheService.getScriptCache();
  const cacheKey = 'grp_v3_' + email.toLowerCase();

  const cached = cache.get(cacheKey);
  if (cached !== null) {
    return cached === 'true';
  }

  let result = false;
  try {
    const response = People.People.searchDirectoryPeople({
      query: email,
      readMask: 'emailAddresses',
      sources: ['DIRECTORY_SOURCE_TYPE_DOMAIN_PROFILE']
    });
    const found = response.people && response.people.some(p =>
      p.emailAddresses && p.emailAddresses.some(e =>
        e.value.toLowerCase() === email.toLowerCase()
      )
    );
    result = !found; // not found as person = group
  } catch (e) {
    result = false;
  }

  cache.put(cacheKey, String(result), GROUP_CACHE_TTL);
  return result;
}

// Get comment override from RSVP response
function getCommentOverride(event) {
  try {
    const eventId = event.getId().split('@')[0];
    const calEvent = Calendar.Events.get('primary', eventId);
    if (calEvent.attendees) {
      const self = calEvent.attendees.find(a => a.self);
      if (self && self.comment) {
        const comment = self.comment.toLowerCase();
        if (comment.includes('#ignore')) return 'ignore';
        if (comment.includes('#1:1') || comment.includes('#1x1')) return '1:1';
        if (comment.includes('#group')) return 'group';
        if (comment.includes('#solo')) return 'solo';
      }
    }
  } catch (e) {
    // ignore
  }
  return null;
}

// Analyze attendees: returns { individuals, groups, hasGroups }
function analyzeAttendees(event) {
  const myEmail = Session.getActiveUser().getEmail().toLowerCase();

  // Build set of all attendees: guests + organizer
  const emails = new Set();
  for (const guest of event.getGuestList()) {
    emails.add(guest.getEmail().toLowerCase());
  }
  const creators = event.getCreators();
  if (creators.length > 0) {
    emails.add(creators[0].toLowerCase());
  }

  let individuals = 0;
  let groups = 0;

  for (const email of emails) {
    if (email === myEmail) continue;
    if (email.includes('@resource.calendar.google.com')) continue;

    if (isGroup(email)) {
      groups++;
    } else {
      individuals++;
    }
  }

  return { individuals, groups, hasGroups: groups > 0 };
}

// Determine event type: 'solo', '1:1', 'group'
function getEventType(event) {
  const { individuals, hasGroups } = analyzeAttendees(event);

  // group = 2+ individuals OR any groups
  if (individuals >= 2 || hasGroups) {
    return 'group';
  }
  // 1:1 = exactly 1 other individual
  if (individuals === 1) {
    return '1:1';
  }
  // solo = just you
  return 'solo';
}

const RULES = [
  // Title-based rules (checked first)
  {
    name: 'out-of-office',
    match: (event) => {
      const title = event.getTitle().toLowerCase();
      return title === 'out of office' || title.startsWith('ooo');
    },
    colorId: '1' // lavender
  },
  {
    name: 'async',
    match: (event) => {
      const title = event.getTitle().toLowerCase();
      return title.includes('[async]');
    },
    colorId: '2' // sage
  },
  {
    name: 'interview',
    match: (event) => {
      const title = event.getTitle().toLowerCase();
      return title.includes('interview');
    },
    colorId: '3' // grape
  },
  {
    name: 'focus-block',
    match: (event) => {
      const title = event.getTitle();
      const guests = event.getGuestList();
      return title === 'Busy' && guests.length === 0;
    },
    colorId: '9' // blueberry
  },
  // Attendee-based rules
  {
    name: '1:1',
    match: (event) => getEventType(event) === '1:1',
    colorId: '5' // banana
  },
  {
    name: 'group-organizer',
    match: (event) => getEventType(event) === 'group' && event.isOwnedByMe(),
    colorId: '8' // graphite
  },
  {
    name: 'group-guest',
    match: (event) => getEventType(event) === 'group' && !event.isOwnedByMe(),
    colorId: '7' // peacock
  }
  // solo events: no rule, keep default color
];

function colorize() {
  const startTime = Date.now();
  const calendar = CalendarApp.getDefaultCalendar();
  const now = new Date();

  // Start of current week (Sunday)
  const startOfWeek = new Date(now);
  startOfWeek.setDate(now.getDate() - now.getDay());
  startOfWeek.setHours(0, 0, 0, 0);

  // End of 4th week (current + 3 more)
  const endDate = new Date(startOfWeek);
  endDate.setDate(startOfWeek.getDate() + 28);

  const events = calendar.getEvents(startOfWeek, endDate);

  // Shuffle with seeded RNG (changes each hour)
  const seed = Math.floor(Date.now() / 3600000);
  const seededRandom = (function(s) {
    return function() {
      s = (s * 1103515245 + 12345) & 0x7fffffff;
      return s / 0x7fffffff;
    };
  })(seed);
  for (let i = events.length - 1; i > 0; i--) {
    const j = Math.floor(seededRandom() * (i + 1));
    [events[i], events[j]] = [events[j], events[i]];
  }

  let updated = 0;

  for (const event of events) {
    if (Date.now() - startTime > MAX_RUNTIME_MS) {
      Logger.log(`Hit max runtime (${MAX_RUNTIME_MS}ms)`);
      break;
    }

    const title = event.getTitle();

    // Check comment override
    const override = getCommentOverride(event);
    if (override === 'ignore') {
      continue;
    }

    // Find matching rule
    for (const rule of RULES) {
      let matches = false;

      // Comment overrides for type
      if (override === '1:1' && rule.name === '1:1') {
        matches = true;
      } else if (override === 'group') {
        if (rule.name === 'group-organizer' && event.isOwnedByMe()) matches = true;
        if (rule.name === 'group-guest' && !event.isOwnedByMe()) matches = true;
      } else if (!override) {
        matches = rule.match(event);
      }

      if (matches) {
        if (event.getColor() !== rule.colorId) {
          event.setColor(rule.colorId);
          Logger.log(`"${title}" -> ${rule.name}`);
          updated++;
        }
        break;
      }
    }
  }

  Logger.log(`Updated ${updated}/${events.length} events`);
  return updated;
}

function createTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'colorize') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('colorize')
    .timeBased()
    .everyHours(1)
    .create();

  Logger.log('Created hourly trigger for colorize');
}

function setup() {
  createTrigger();
  colorize();
}

// For testing
return { RULES, isGroup, getEventType };
