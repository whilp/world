# I created this so that my page up/down keys would be loaded on boot.
# wcmaier / 2005.06.16

setkeycodes e06a 127
setkeycodes e069 126

echo "keycode 127 = Next" | loadkeys
echo "keycode 126 = Prior" | loadkeys
echo "keycode 1 = Caps_Lock" | loadkeys
echo "keycode 58 = Escape" | loadkeys
