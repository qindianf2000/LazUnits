# Configuring the Windows certificate store

In order to be able to use the certificate for the website, the certificates need to be imported into the Windows certificate store.

When you open the start menu in Windows 10 and you type "certificates", Windows comes up with two relevant suggestions:
- Manage computer certificates
- Manage user certificates
Both will be needed to install the SSL certificate.

## Computer certificates

When the context menu for Personal is accessed there is an option "Import" under All Tasks.
Selecting this item will start a wizard to select and import a certificate.
In this certificate store both the rootCA.pem and server.pfx certificate need to be imported.
By importing server.pfx the SSL certificate becomes selectable in IIS.
Importing rootCA.pem will stop IIS from generating warnings the certificate chain is not complete.

At this point, when there is an HTTPS binding and you would try to visit https://acme-site.dev using Chrome in Windows, you would still see an warning page instead of the website itself. This is because Windows still needs to be told it can trust certificates signed with the self created root certificate.

## Personal certificates (user certificates)

In order to inform Windows it can trust certificates issued with the self created root certificate, the root certificate should be imported under personal certificates. This application looks the same as the one for managing the computer certificates. The big difference is the location where the root certificate should be imported into: Trusted Root Certification Authorities.

Importing the rootCA.pem certificate in this location will be met with a warning message. It informs that accepting an CA certificate from an unknown origin is dangerous and to make sure the certificate is actually legit.

Since the certificate being added to the certificate store is the self signed certificate this dialog can safely be answered with Yes. With the root certificate added to the list of trusted root certification authorities all the steps are done. Opening https://acme-site.dev will no longer display any warnings, instead Chrome will display a nice "secure" status in the URL bar.

## Other

See the website for instructions on MacOS and Firefox.

## Credits

Thanks to Thijs Busser and his blog entry at https://medium.com/@tbusser/creating-a-browser-trusted-self-signed-ssl-certificate-2709ce43fd15