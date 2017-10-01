#!/usr/bin/env python

import os
import subprocess

def mailpasswd():
    path = "/home/david/.password-store/web/gmail.gpg"
    args = ["gpg2", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""

if __name__ == "__main__":
    print mailpasswd()
