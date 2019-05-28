[![Donate](https://img.shields.io/badge/Donate-PayPal-red.svg)](https://www.paypal.me/JimDreherHome)

# Install Indy 10 on Lazarus

This is an overview of how to install Indy10 on Lazarus, both design-time and run-time.

## Use the Online Package Manager (OPM)
- Install Online Package Manager (http://wiki.lazarus.freepascal.org/Online_Package_Manager)
- Start OPM (Lazarus menu: Package, Online Package Manager)
- Filter by 'indy'
- Select the package file: indy10
- Expand to verify version 10.6.2.5494 or later

## Run-time
- Continue from Use OPM above...
- Press Download to download indy10.zip
- Unzip to your packages folder e.g. C:\lazarus\packages\indy10
- Open your project
- Lazarus menu: Package, Open Package File (*.lpk)
- Choose the indy10 package e.g. C:\lazarus\packages\indy10\indylaz.lpk
- The Package Viewer will open the package
- Packager Viewer menu: Use, Add to Project (no need to compile yet)
- The Project Inspector will show Required Packages: indylaz
- Compile your project and it will resolve the Indy unit include statements

## Design-time
- Continue from Use OPM above...
- Press Install to install indy10
- Complete the rebuild and restart IDE
- You will now have many tabs with Indy design-time components

## Fix Memory Leaks
- There is a known memory leak issue
- Use the tool Indy10FixMemLeak to resolve the packages on your system

## OpenSSL	

- The OpenSSL site is: https://www.openssl.org/
- Pre-compiled binaries: https://wiki.openssl.org/index.php/Binaries
- My demos use **Openssl-1.0.2l-i386-win32**
	
## My Config

- Win10 Home, Lazarus v1.8.4 with Free Pascal v3.0.4
- Indy10.6.2.5494
- Openssl-1.0.2l-i386-win32

### Donations

If this units are useful, or if the source code helps you in some way, then a small donation would be appreciated.  Just click on the "donation" button above.  Your donation is not tax deductible, but will be used to help promote freeware from myself and other software authors.
