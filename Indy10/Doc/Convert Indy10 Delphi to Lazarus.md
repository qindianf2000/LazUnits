[![Donate](https://img.shields.io/badge/Donate-PayPal-red.svg)](https://www.paypal.me/JimDreherHome)

# Convert Indy10 Delphi to Lazarus projects

These tips will help you successfully convert Indy10 Delphi to Lazarus projects

- Most Delphi demos from the Internet include Indy VCL objects in the .dfm form.
- My preference is not to use the Indy Lazarus package because it adds over a dozen tabs with VCL objects to the Lazarus IDE.
- This explains how to remove the VCL objects after converting from Delphi to Lazarus.

## Convert Delphi to Lazarus
- Use the Lazarus "Convert Delphi Project" feature: http://wiki.freepascal.org/Delphi_Converter_in_Lazarus
- My preference is to UN-Check Cross-Platform and Support Delphi, use other defaults

## Remove VCL Objects
- Close Lazarus then open the .frm file with a text editor
- Save a backup copy, just in case
- Cut all the Indy objects from the .frm file
- Open the converted Lazarus project
- If not exists, insert a Form1.Create function
- Paste the text with the Indy objects into the Form1.Create function
- Cleanup the code, e.g. change "=" to ":=", and add a ";" at the end of the line
- Example of Indy object cut from the Delphi .dfm

		object IdTCPServer1: TIdTCPServer
			Bindings = <>
			DefaultPort = 0
			OnConnect = IdTCPServer1Connect
			OnDisconnect = IdTCPServer1Disconnect
			OnException = IdTCPServer1Exception
			OnExecute = IdTCPServer1Execute
			Left = 16
			Top = 264
		end

- Example of Indy object pasted into the Lazarus .frm and cleaned up

		procedure TStringServerForm.FormCreate(Sender: TObject);
		begin
			IdTCPServer1:=TIdTCPServer.Create;
			with IdTCPServer1 do begin
			  DefaultPort := 0;
			  OnConnect := IdTCPServer1Connect;
			  OnDisconnect := IdTCPServer1Disconnect;
			  OnException := IdTCPServer1Exception;
			  OnExecute := IdTCPServer1Execute;
			  //REMOVE VCL object setting: Left = 16
			  //REMOVE VCL object setting: Top = 264
			end;
		end;
	
## Before Compiling
- Lazarus Options, Project Options, Miscellaneous: Check "Main unit is Pascal source", others are personal preference.
- Lazarus Options, Compiler Options, Paths: set the Lazarus project path to your Indy10 install
- Set the same path for both the "Other Unit Files" and "Include Files"
- Example: E:\Indy10\Core;E:\Indy10\Protocols;E:\Indy10\Security;E:\Indy10\System


## Compile and Verify
- Compile and resolve any issues
- Some demos my be older than Indy 10.6 so update accordingly
- Run and verify the demos
- Resolve any issues
  
## My Config

- Win10 Home, Lazarus v1.8.4 with Free Pascal v3.0.4
- Indy10.6.2.5494 from indy.fulgan.com

### Donations

If this units are useful, or if the source code helps you in some way, then a small donation would be appreciated.  Just click on the "donation" button above.  Your donation is not tax deductible, but will be used to help promote freeware from myself and other software authors.
