#!/usr/bin/env sh

qemu-system-x86_64 \
  -m 4G \
  -cpu host \
  -smp 2 \
  -accel hvf \
  -drive file=debian.qcow2,if=virtio \
  -cdrom debian-13.3.0-amd64-netinst.iso \
  -net nic,model=virtio -net user \
  -device virtio-tablet-pci \
  -vga virtio \
  -display cocoa,show-cursor=on
