NOTE: this software is posted here for archival and research purposes.
This version of TFS was created as a testbed for TFS concepts. It should be viewed as a prototype, and should not be used for valuable data. Utilities such as fdisk were not needed in our evaluation of TFS, and have received only minimal testing.

-----

TFS is a file system modification intended to support contributory storage applications, such as Freenet. These applications allow users to contribute storage from their desktop to a shared pool of resources. Like all contributory applications, contributory storage systems must not interfere with the user in any way. Because resources are being donated voluntarily, the user will simply turn off or uninstall the application if it get in their way. To overcome this, contributory systems try to be transparent, i.e. the user should not notice any difference in the behavior of their own applications as a result of contributing resources. For contributory storage, this usually means the application must limit itself to contributing a small amount of space.

TFS provides a new type of file, called transparent files, which can be used by contributory applications to avoid interference. From the perspective of ordinary files, transparent files take up no space on the disk, and cause no fragmentation. If an ordinary file tries to allocate a block which is being used for a transparent file, the transparent file will be overwritten.

-----

For more details on TFS, read the [https://github.com/plasma-umass/transparentFS/blob/master/TFS.pdf](technical paper) or the [https://github.com/plasma-umass/transparentFS/blob/master/tfs-fast07.pdf](presentation slides).

