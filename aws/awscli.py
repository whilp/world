import os
import sys

def main():
    if os.environ.get('LC_CTYPE', '') == 'UTF-8':
        os.environ['LC_CTYPE'] = 'en_US.UTF-8'
    
    import awscli.clidriver
    awscli.clidriver.main()


if __name__ == '__main__':
    import awscli.clidriver
    sys.exit(main())
