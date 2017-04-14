A simple FAT32 bootloader written in assembly to boot from USB drives.

## What this bootloader can do:

It can load the first file( which should be a file less than 4096 bytes) which is located in a USB drive with a FAT32 file
system to the memory and hand over the execution to that program.

## Steps to use the Bootloader:

1. Compile the bootloader.<br>
        `nasm <path of the bootloader> ­f bin ­o boot.bin`
        
2. Unmount the USB device.<br>
        `sudo umount <absolute path of the device>`
        
3. Format the USB device to FAT32 file system.<br>
        `sudo /sbin/mkdosfs ­F 32 <absolute path of the device>`
        
4. Copy the Bootloader to the USB device.<br>
        `sudo dd if=boot.bin of=<absolute path of the device>`
        
5. Copy the compiled kernel to the USB device (just as you copy any usual file to the USB device).

6. Restart the computer and boot from the USB device!.

More information can be found in the pdf in the doc folder. Don't forget to leave a comment in http://blog.ishans.info/2012/01/19/a-simple-fat32-bootloader-written-in-assembly-to-boot-from-usb-drives/

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/c61d5574272fc7a7e348d2ad649a183e "githalytics.com")](http://githalytics.com/ishanthilina/USB-FAT32-Bootloader)

