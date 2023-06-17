# Scripts

## Keyboard Backlight helper

Script written by ChatGPT. It runs on Asahi linux and checks wether the keyboard has been inactive for more than X seconds to turn it off. Also after you press a key again it would enable brightness again.

This script must be run as a service using `systemd`. Example of the service code:

```
[Unit]
Description=Keyboard Backlight Service

[Service]
User=root
Group=root
ExecStart=/usr/bin/python3 /home/monkey/Desktop/keyboard_backlight.py

[Install]
WantedBy=default.target
```
