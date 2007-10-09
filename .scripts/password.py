#!/usr/bin/env python

import random
import string

# Build a list of safe characters to use when generating password
# strings.
SKIP_CHARS = '\t\r\n \x0b\x0c'
CHARACTERS = [x for x in string.printable if x not in SKIP_CHARS]

# Give up if we can't generate a password in 10 attempts.
MAX_ATTEMPTS = 10 

class RepetitionError(Exception):
    """Raised when password generation takes too long."""

class PasswordGenerator(object):
    """Generates passwords."""

    def __init__(self, length=12):
        self.length = length
        self.chars = CHARACTERS
        self.max_attempts = MAX_ATTEMPTS

    def generate(self):
        """Generate a password."""
        password = []

        while len(password) < self.length - 1:
            char = random.choice(self.chars)

            if self.char_isok(char):
                password.append(char)

        # Keep generating passwords until we get one that fits the
        # policy.
        i = 0
        while not self.pass_isok(password) and i < self.max_attempts:
            password = self.generate()
            i += 1

        if i == self.max_attempts:
            raise RepetitionError("Failed to generate a password in"
                    "fewer than %d steps" % self.max_attempts)

        return password

    def char_isok(self, char):
        """Return True if the supplied character is 'OK'.

        This method can be overridden to adjust the policy of the
        Generator.
        """

        return True

    def pass_isok(self, password):
        """ Return True if the supplied password is 'OK'.

        This method can be overridden to adjust the policy of the
        Generator.
        """

        return True

def has_digit(_str, num=-1):
    """Return True if there are at least 'num' digits in '_str'.

        >>> has_digit('asdf', 1)
        False
        >>> has_digit('asdf')
        True
        >>> has_digit('asdf123', 3)
        True
    """
    if len([x for x in _str if x in string.digits]) >= num:
        return True
    else:
        return False

def has_letter(_str, num=-1):
    """Return True if there are at least 'num' letters in '_str'.

        >>> has_letter('1231', 1)
        False
        >>> has_letter('1232')
        True
        >>> has_letter('12dadf1', 3)
        True
    """
    if len([x for x in _str if x in string.letters]) >= num:
        return True
    else:
        return False

def has_variety(_str, num=2, consecutive=True):
    """Return True if no character repeats more than 'num' times.

    If 'consecutive' is True (as it is by default), non-consecutive
    repetition is tolerated. Values of 'num' less than 2 (the
    default) are meaningless; similarly, strings shorter than 'num'
    aren't interesting. In either case, return True.

        >>> has_variety('assss1lk3j')
        False
        >>> has_variety('akdlekcada')
        True
        >>> has_variety('adddeadf', 4)
        True
        >>> has_variety('ababababa', 2, consecutive=False)
        False
    """
    if num < 2 or len(_str) < num:
        return True

    if consecutive:
        for i, char in enumerate(_str):
            # Check that we're far enough into the string to make a
            # useful check.
            if i < num:
                continue

            # If we only have one unique character in the last 'num'
            # characters, we have a problem.
            if len(set(_str[i - num:i])) == 1:
                return False
    else:
        d = {}
        for char in _str:
            if char in d:
                d[char] += 1
            else:
                d[char] = 0

        if [x for x in d.values() if x > num]:
            return False

    # If we made it this far, the string meets our policy.
    return True

def mq_filter(password):
    """Filter for MortgageQuestions.com.

        >>> mq_filter(15 * 'a') # Too long.
        False
        >>> mq_filter(6 * 'a')  # Too short.
        False
        >>> mq_filter('12345678')   # Doesn't have a letter.
        False
        >>> mq_filter('aAdfceadf')  # Doesn't have a digit.
        False
        >>> mq_filter('a31dd39dka') # Doesn't have enough variety.
        False
        >>> mq_filter('d91*adksd')  # Has a naughty character.
        False
        >>> mq_filter('V08d1%2k3')  # Should be OK.
        True
    """
    if len(password) < 7 or len(password) > 14:
        return False
    elif not has_digit(password, 1):
        return False
    elif not has_letter(password, 1):
        return False
    elif not has_variety(password, 2):
        return False
    elif [x for x in password if x in '&*|']:
        # Uh-oh; we have a naughty char.
        return False

    # We made it this far; looks good.
    return True

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    import sys

    import logging

    # Settings.
    LOG_FMT = '%(message)s'
    LENGTH = 11
    NUM_PASSWORDS = 10

    # Set up logging.
    log = logging.getLogger('.')
    log.setLevel(logging.DEBUG)

    formatter = logging.Formatter(LOG_FMT)

    streamhandler = logging.StreamHandler()
    streamhandler.setFormatter(formatter)
    log.addHandler(streamhandler)

    args = sys.argv[1:]

    try:
        LENGTH = int(args[0])
    except IndexError:
        # We'll settle for the default length, then.
        pass

    log.debug("Generating passwords with a length of %d", LENGTH)
    pgen = PasswordGenerator(LENGTH)
    pgen.pass_isok = mq_filter

    # Generate the passwords.
    for i in range(NUM_PASSWORDS):
        password = ''.join(pgen.generate())
        print password, mq_filter(password)
