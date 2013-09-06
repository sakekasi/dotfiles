source /etc/os-release
distro=$NAME

arch="Arch Linux"
ubuntu="Ubuntu"

if [ "$distro" == "$arch" ]; then
	echo 'arch'
	xmonad
else
	echo 'unknown distro'
fi
