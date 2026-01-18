const OTHER_CALENDAR_PROP = 'other_calendar_email';

function getOtherCalendar() {
  return PropertiesService.getScriptProperties().getProperty(OTHER_CALENDAR_PROP);
}

function setOtherCalendar(email) {
  PropertiesService.getScriptProperties().setProperty(OTHER_CALENDAR_PROP, email);
  Logger.log(`Set other calendar to: ${email}`);
}

function autoAcceptOtherCalendarInvites() {
  const otherEmail = getOtherCalendar();
  if (!otherEmail) {
    Logger.log('No other calendar configured. Run setOtherCalendar(email) first.');
    return;
  }

  const calendar = CalendarApp.getDefaultCalendar();
  const now = new Date();
  const fourWeeksOut = new Date(now.getTime() + 28 * 24 * 60 * 60 * 1000);

  const events = calendar.getEvents(now, fourWeeksOut);
  let accepted = 0;

  for (const event of events) {
    const organizer = event.getCreators()[0];
    if (organizer && organizer.toLowerCase() === otherEmail.toLowerCase()) {
      const status = event.getMyStatus();
      if (status === CalendarApp.GuestStatus.INVITED ||
          status === CalendarApp.GuestStatus.MAYBE) {
        event.setMyStatus(CalendarApp.GuestStatus.YES);
        Logger.log(`Accepted: ${event.getTitle()} on ${event.getStartTime()}`);
        accepted++;
      }
    }
  }

  Logger.log(`Auto-accepted ${accepted} events from ${otherEmail}`);
  return accepted;
}

function createAutoAcceptTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'autoAcceptOtherCalendarInvites') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('autoAcceptOtherCalendarInvites')
    .timeBased()
    .everyHours(1)
    .create();

  Logger.log('Created hourly trigger for autoAcceptOtherCalendarInvites');
}

function onCalendarChangeAutoAccept(e) {
  autoAcceptOtherCalendarInvites();
}

function createAutoAcceptCalendarTrigger() {
  const triggers = ScriptApp.getProjectTriggers();
  for (const trigger of triggers) {
    if (trigger.getHandlerFunction() === 'onCalendarChangeAutoAccept') {
      ScriptApp.deleteTrigger(trigger);
    }
  }

  ScriptApp.newTrigger('onCalendarChangeAutoAccept')
    .forUserCalendar(Session.getActiveUser().getEmail())
    .onEventUpdated()
    .create();

  Logger.log('Created calendar change trigger for auto-accept');
}

function setupAutoAccept() {
  const otherEmail = getOtherCalendar();
  if (!otherEmail) {
    Logger.log('ERROR: Run setOtherCalendar("your@other.email") first');
    return;
  }

  createAutoAcceptTrigger();
  createAutoAcceptCalendarTrigger();
  autoAcceptOtherCalendarInvites();
}

// For testing
return { getOtherCalendar, setOtherCalendar };
