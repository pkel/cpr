# textidote.nix
#
# Packages github.com/sylvainhalle/textidote
#
# You can build this file with the following command:
#
#   nix-build textidote.nix
#
# This will create the symlink result in the current directory.  The
# runnable shell script is result/bin/GINsim.
#
# You can use this file in a nix-shell, as shell.nix illustrates.

# Copied from:
# https://git.marvid.fr/scolobb/nix-GINsim/src/commit/d7147baefccad2e6643fadc3600b76e8dbd2d67a

# Copyright 2020 by Sergiu Ivanov <sivanov@colimite.fr>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

let
  # Import nixpkgs to be able to supply reasonable default values for
  # the anonymous function this file defines.
  pkgs = import <nixpkgs> {};
in
# These arguments define the resources (packages and native Nix tools)
# that will be used by the package (the anonymous function I define).
# I want this file to be buildable directly using the command
# nix-build textidote.nix, so I have to supply reasonable default values
# to these arguments.  The default values naturally come from the
# corresponding attributes of Nixpkgs, visible here under the binding
# pkgs.
{ stdenv ? pkgs.stdenv
, fetchurl ? pkgs.fetchurl
, makeWrapper ? pkgs.makeWrapper
, jre ? pkgs.jre
}:

# I'll use the default builder, because I don't need any particular
# features.
stdenv.mkDerivation rec {
  name = "textidote";
  version = "0.8.3";

  # Simply fetch the JAR file from Github
  src = fetchurl {
    url = "https://github.com/sylvainhalle/textidote/releases/download/v${version}/textidote.jar";
    sha256 = "BIYswDrVqNEB+J9TwB0Fop+AC8qvPo53KGU7iupC7tk=";
  };
  # I fetch the JAR file directly, so no archives to unpack.
  dontUnpack = true;

  # I need makeWrapper in my build environment to generate the wrapper
  # shell script.  This shell script will call the Java executable on
  # the JAR file of GINsim and will set the appropriate environment
  # variables.
  nativeBuildInputs = [ makeWrapper ];

  # The only meaningful phase of this build.  I create the
  # subdirectory share/java/ in the output directory, because this is
  # where JAR files are typically stored.  I also create the
  # subdirectory bin/ to store the executable shell script.  I then
  # copy the downloaded JAR file to $out/share/java/.  Once this is
  # done, I create the wrapper shell script using makeWrapper.  This
  # script wraps the Java executable (${jre}/bin/java) in the output
  # shell script file $out/bin/textidote.  The script adds the argument
  # -jar … to the Java executable, thus pointing it to the actual
  # textidote JAR file.
  installPhase = ''
  mkdir -pv $out/share/java $out/bin
  cp ${src} $out/share/java/${name}-${version}.jar
  makeWrapper ${jre}/bin/java $out/bin/textidote \
    --add-flags "-jar $out/share/java/${name}-${version}.jar"
  '';

  # Some easy metadata, in case I forget.
  meta = {
    homepage = "https://sylvainhalle.github.io/textidote/";
    description = "A correction tool for LaTeX documents";
    license = pkgs.lib.licenses.gpl3;
    platforms = pkgs.lib.platforms.unix;
  };
}

