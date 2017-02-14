8 August 2016, Scot Martin, for HDF.PInvoke -Version 1.8.17.4

The HDF group explains that the C code has been compiled and can be accessed through .NET using PInvoke. Read here:
https://www.hdfgroup.org/projects/hdf.net/
https://www.nuget.org/packages/HDF.PInvoke/1.8.17.4

How to make this work?

1. First we need to get the latest version of PInvoke, which we'll get from 'nuget'.

2. Download the appropriate 'nuget': https://dist.nuget.org/index.html

3. Put this file where you want to access it within a windows CMD window, e.g., in the default directory that CMD opens to, which is "C:\Users\Scot Martin" in my case (https://en.wikipedia.org/wiki/Cmd.exe). See some example output at EXHIBIT A.

4. Execute the installation command: 'C:\Users\Scot Martin>nuget install HDF.PInvoke -Version 1.8.17.4 -OutputDirectory "C:\Users\Scot Martin\test"' (https://www.nuget.org/packages/HDF.PInvoke/1.8.17.1). See EXHIBIT B of CMD window.

5. Buried down in the 'test' directory you will find the 'lib' directory. That directory and its contents are what we are after. Take that stuff and put it in the directory of the HDF5 Mathematica files that we are going to use. You can delete everything else. (You will end up choosing either the 32 or 64 bin directory. The example herein is for the provided files is for Windows 64.)

6. IMPORTANT HINTS: See 'TROUBLESHOOTING.txt' file for solutions to problems below.
6a. "System.IO.FileLoadException: Could not load file or assembly 'file:///C:\...\HDF.PInvoke\lib\NET45\HDF.PInvoke.dll' or one of its dependencies. Operation is not supported. (Exception from HRESULT: 0x80131515)"
6b. "A .NET exception occurred: ... System.DllNotFoundException: Unable to load DLL 'hdf5.dll': The specified module could not be found. (Exception from HRESULT: 0x8007007E)"


EXHIBIT A: 'nuget' is installed and ready to go through CMD window.

C:\Users\Scot Martin>nuget
NuGet Version: 3.4.4.1321
usage: NuGet <command> [args] [options]
Type 'NuGet help <command>' for help on a specific command.

Available commands:

 add         Adds the given package to a hierarchical source. http sources are
             not supported. For more info, goto https://docs.nuget.org/consume/
             command-line-reference#add-command.

 config      Gets or sets NuGet config values.

 delete      Deletes a package from the server.

 help (?)    Displays general help information and help information about other
             commands.

 init        Adds all the packages from the <srcPackageSourcePath> to the hiera
             rchical <destPackageSourcePath>. http feeds are not supported. For
             more info, goto https://docs.nuget.org/consume/command-line-refere
             nce#init-command.

 install     Installs a package using the specified sources. If no sources are
             specified, all sources defined in the NuGet configuration file are
             used. If the configuration file specifies no sources, uses the def
             ault NuGet feed.

 list        Displays a list of packages from a given source. If no sources are
             specified, all sources defined in %AppData%\NuGet\NuGet.config are
             used. If NuGet.config specifies no sources, uses the default NuGet
             feed.

 locals      Clears or lists local NuGet resources such as http requests cache,
             packages cache or machine-wide global packages folder.

 pack        Creates a NuGet package based on the specified nuspec or project f
             ile.

 push        Pushes a package to the server and publishes it.
             NuGet's default configuration is obtained by loading %AppData%\NuG
             et\NuGet.config, then loading any nuget.config or .nuget\nuget.con
             fig starting from root of drive and ending in current directory.

 restore     Restores NuGet packages.

 setApiKey   Saves an API key for a given server URL. When no URL is provided A
             PI key is saved for the NuGet gallery.

 sources     Provides the ability to manage list of sources located in %AppData
             %\NuGet\NuGet.config

 spec        Generates a nuspec for a new package. If this command is run in th
             e same folder as a project file (.csproj, .vbproj, .fsproj), it wi
             ll create a tokenized nuspec file.

 update      Update packages to latest available versions. This command also up
             dates NuGet.exe itself.

For more information, visit http://docs.nuget.org/docs/reference/command-line-reference


EXHIBIT B:

C:\Users\Scot Martin>nuget install HDF.PInvoke -Version 1.8.17.4 -OutputDirectory "C:\Users\Scot Martin\test" 
Feeds used:
  C:\Users\Scot Martin\AppData\Local\NuGet\Cache
  https://api.nuget.org/v3/index.json

Attempting to gather dependency information for package 'HDF.PInvoke.1.8.17.4' with respect to project 'C:\Users\Scot Martin\test', targeting 'Any,Version=v0.0'
Attempting to resolve dependencies for package 'HDF.PInvoke.1.8.17.4' with DependencyBehavior 'Lowest'
Resolving actions to install package 'HDF.PInvoke.1.8.17.4'
Resolved actions to install package 'HDF.PInvoke.1.8.17.4'
  GET https://api.nuget.org/packages/hdf.pinvoke.1.8.17.4.nupkg
  OK https://api.nuget.org/packages/hdf.pinvoke.1.8.17.4.nupkg 23ms
Acquiring lock for the installation of HDF.PInvoke 1.8.17.4
Acquired lock for the installation of HDF.PInvoke 1.8.17.4
Installing HDF.PInvoke 1.8.17.4.
Completed installation of HDF.PInvoke 1.8.17.4
Adding package 'HDF.PInvoke.1.8.17.4' to folder 'C:\Users\Scot Martin\test'
Added package 'HDF.PInvoke.1.8.17.4' to folder 'C:\Users\Scot Martin\test'
Successfully installed 'HDF.PInvoke 1.8.17.4' to C:\Users\Scot Martin\test


