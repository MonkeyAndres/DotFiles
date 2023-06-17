import time
import subprocess
from evdev import InputDevice, ecodes
from threading import Thread

# Set the time threshold for inactivity (in seconds)
INACTIVITY_THRESHOLD = 120

# Set the brightness value for enabled backlight
BACKLIGHT_ON_BRIGHTNESS = "100"

# Set the brightness value for disabled backlight
BACKLIGHT_OFF_BRIGHTNESS = "0"

# Set the path to the keyboard input device
KEYBOARD_DEVICE_PATH = "/dev/input/event0"
# Replace "X" with the appropriate event number for your keyboard device
# You can find the correct event number by running the command: "cat /proc/bus/input/devices"

# Initialize the last active time
last_active_time = time.time()

# Function to set the backlight brightness
def set_backlight_brightness(brightness):
    brightness_path = "/sys/class/leds/kbd_backlight/brightness"
    with open(brightness_path, "w") as file:
        file.write(brightness)

# Function to handle keyboard events
def keyboard_event_handler():
    global last_active_time
    keyboard = InputDevice(KEYBOARD_DEVICE_PATH)

    for event in keyboard.read_loop():
        if event.type == ecodes.EV_KEY and event.value == 1:  # Key press event
            last_active_time = time.time()
            set_backlight_brightness(BACKLIGHT_ON_BRIGHTNESS)

# Create a separate thread for handling keyboard events
keyboard_thread = Thread(target=keyboard_event_handler)
keyboard_thread.daemon = True
keyboard_thread.start()

# Main loop for checking inactivity
while True:
    if time.time() - last_active_time >= INACTIVITY_THRESHOLD:
        set_backlight_brightness(BACKLIGHT_OFF_BRIGHTNESS)
    time.sleep(1)
