#!/usr/bin/env sh

qemu-system-x86_64 \
  -m 4G \
  -cpu host \
  -smp 2 \
  -accel hvf \
  -drive file=debian.qcow2,if=virtio \
  -netdev user,id=net0,hostfwd=tcp::2222-:22 \
  -device virtio-net-pci,netdev=net0 \
  -vga virtio \
  -display cocoa,show-cursor=on \
  -device virtio-tablet-pci
