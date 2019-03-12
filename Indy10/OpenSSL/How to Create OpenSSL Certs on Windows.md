# Creating OpenSSL Certs on Windows

The following files are used to create a localhost certificate for demo and testing purposes

- createkeys.cmd
- createkeys.cnf
- v3.ext

## Instructions

- Edit v3.ext and change if necessary
- Edit createkeys.cnf
- Execute createkeys.cmd from the command line

The following files will be generated;

- rootCA.pem
- rootCA.key
- server.crt
- server.key
- server.pfx

## Important
To view the keys, used viewkeys.cmd

Read the doc "How to configure OpenSSL Certs on Windows"

See the source code in the demos how to use the certificates above.

## Credits

Thanks to Thijs Busser and his blog entry at https://medium.com/@tbusser/creating-a-browser-trusted-self-signed-ssl-certificate-2709ce43fd15