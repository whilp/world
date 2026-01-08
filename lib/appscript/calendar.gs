const GROUP_CACHE_TTL = 21600; // 6 hours
const MAX_EVENTS_PER_RUN = 200;
const MAX_RUNTIME_MS = 180000; // 3 minutes

function isGoogleGroup(email) {
  const cache = CacheService.getScriptCache();
  const cacheKey = 'group_' + email.toLowerCase();
  const cached = cache.get(cacheKey);

  if (cached !== null) {
    return cached === 'true';
  }

  let isGroup = false;
  try {
    AdminDirectory.Groups.get(email);
    isGroup = true;
  } catch (e) {
    isGroup = false;
  }

  cache.put(cacheKey, String(isGroup), GROUP_CACHE_TTL);
  return isGroup;
}

function getRealGuestCount(event) {
  const guests = event.getGuestList();
  let groupCount = 0;

  for (const guest of guests) {
    if (isGoogleGroup(guest.getEmail())) {
      groupCount++;
    }
  }

  return {
    total: guests.length,
    individuals: guests.length - groupCount,
    hasGroups: groupCount > 0
  };
}

const RULES = [
  {
    name: 'out-of-office',
    match: (event) => {
      const title = event.getTitle().toLowerCase();
      return title === 'out of office' || title.startsWith('ooo');
    },
    colorId: '1' // lavender (muted, ignorable)
  },
  {
    name: 'async',
    match: (event) => {
      const title = event.getTitle().toLowerCase();
      return title.includes('[async]');
    },
    colorId: '2' // sage (muted - no attendance needed)
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
  {
    name: '1:1',
    match: (event) => {
      const { individuals, hasGroups } = getRealGuestCount(event);
      return individuals === 1 && !hasGroups;
    },
    colorId: '5' // banana
  },
  {
    name: 'group-meeting-organizer',
    match: (event) => {
      const { total, hasGroups } = getRealGuestCount(event);
      return (total >= 2 || hasGroups) && event.isOwnedByMe();
    },
    colorId: '8' // graphite
  },
  {
    name: 'recurring-group-guest',
    match: (event) => {
      const { total, hasGroups } = getRealGuestCount(event);
      return (total >= 2 || hasGroups) && !event.isOwnedByMe() && event.isRecurringEvent();
    },
    colorId: '7' // peacock
  },
  {
    name: 'oneoff-group-guest',
    match: (event) => {
      const { total, hasGroups } = getRealGuestCount(event);
      return (total >= 2 || hasGroups) && !event.isOwnedByMe() && !event.isRecurringEvent();
    },
    colorId: '4' // flamingo
  }
];

const CALENDAR_COLORS = {
  // colorblind-friendly palette (first 5 optimized for contrast)
  blueberry: '9',  // 1. blue - visible to all types
  banana: '5',     // 2. yellow - high brightness, contrasts with blue
  graphite: '8',   // 3. gray - neutral, distinct luminance
  grape: '3',      // 4. purple - distinct from red/green confusion
  peacock: '7',    // 5. teal/cyan - blue-adjacent, different brightness

  // remaining colors (less distinct for colorblind users)
  lavender: '1',
  sage: '2',
  flamingo: '4',
  tangerine: '6',
  basil: '10',
  tomato: '11'
};

function applyColorRules() {
  const startTime = Date.now();
  const calendar = CalendarApp.getDefaultCalendar();
  const now = new Date();
  const twoWeeksAgo = new Date(now.getTime() - 14 * 24 * 60 * 60 * 1000);
  const fourWeeksOut = new Date(now.getTime() + 28 * 24 * 60 * 60 * 1000);

  const events = calendar.getEvents(twoWeeksAgo, fourWeeksOut);

  let updated = 0;
  let processed = 0;
  for (const event of events) {
    if (processed >= MAX_EVENTS_PER_RUN) {
      Logger.log(`Hit max events limit (${MAX_EVENTS_PER_RUN}), stopping`);
      break;
    }
    if (Date.now() - startTime > MAX_RUNTIME_MS) {
      Logger.log(`Hit max runtime (${MAX_RUNTIME_MS}ms), stopping`);
      break;
    }

    processed++;
    for (const rule of RULES) {
      if (rule.match(event)) {
        const currentColor = event.getColor();
        if (currentColor !== rule.colorId) {
          event.setColor(rule.colorId);
          Logger.log(`Applied rule '${rule.name}' to event '${event.getTitle()}' on ${event.getStartTime()}`);
          updated++;
        }
        break;
      }
    }
  }

  Logger.log(`Processed ${processed}/${events.length} events, updated ${updated}`);
  return updated;
}

function createTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'applyColorRules') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('applyColorRules')
    .timeBased()
    .everyHours(1)
    .create();

  Logger.log('Created hourly trigger for applyColorRules');
}

function onCalendarChange(e) {
  applyColorRules();
}

function createCalendarTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'onCalendarChange') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('onCalendarChange')
    .forUserCalendar(Session.getActiveUser().getEmail())
    .onEventUpdated()
    .create();

  Logger.log('Created calendar change trigger');
}

function setup() {
  createTrigger();
  createCalendarTrigger();
  applyColorRules();
}

