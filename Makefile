# Makefile -  The Makefile for cybercritic-microblogger.
# Copyright (C) 2009  Rob Myers rob@robmyers.org
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

cybercritic-microblogger:	make.lisp \
				cybercritic.lisp \
				cybercritic-microblogger.lisp \
				package.lisp \
				cybercritic.asd
	sbcl --load "./make.lisp"

clean:
	rm -f cybercritic-microblogger
	rm -f *.fasl

distclean: clean
