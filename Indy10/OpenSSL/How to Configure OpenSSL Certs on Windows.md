# Configuring the Windows certificate store

Import certificates into the Windows certificate store.

Windows start menu, search "certificates":
- Manage computer certificates
- Manage user certificates
Both will be needed to install the SSL certificate.

## Computer certificates

Personal context menu: All Tasks, Import: rootCA.pem
Personal context menu: All Tasks, Import: server.pfx

rootCA.pem will stop IIS from generating warnings the certificate chain is not complete.
server.pfx allows the SSL certificate to be selectable in IIS.

At this point, try to visit https://acme-site.dev using Chrome in Windows.
You will see a warning page instead of the website itself.
Windows still needs to be told it can trust certificates signed with the self created root certificate.

## Personal certificates (user certificates)

Personal context menu: All Tasks, Import: rootCA.pem
Import into: Trusted Root Certification Authorities.

You will be met with a warning message that accepting an CA certificate from an unknown origin is dangerous and to make sure the certificate is actually legit - answer Yes.

Opening https://acme-site.dev will no longer display any warnings.
Instead Chrome will display a nice "secure" status in the URL bar.

## Other

See the website for instructions on MacOS and Firefox.

## Credits

Thanks to Thijs Busser and his blog entry at https://medium.com/@tbusser/creating-a-browser-trusted-self-signed-ssl-certificate-2709ce43fd15