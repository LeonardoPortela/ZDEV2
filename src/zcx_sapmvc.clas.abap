class ZCX_SAPMVC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

*"* public components of class ZCX_SAPMVC
*"* do not include other source files here!!!
public section.

  constants DYNPRO_MISSING type SOTR_CONC value 'DD51250CFAA2D9F19935000C292086BE'. "#EC NOTEXT
  constants PROGRAM_MISSING type SOTR_CONC value 'DD51250CFAA2E1F19935000C292086BE'. "#EC NOTEXT
  constants MODEL_INSTANCE_MISSING type SOTR_CONC value 'DD51250CFAA2DDF19935000C292086BE'. "#EC NOTEXT
  constants CONTROLLER_INSTANCE_NOT_FOUND type SOTR_CONC value 'DD512507025F88F19935000C292086BE'. "#EC NOTEXT
  constants CONTROLLER_INSTANCE_MISSING type SOTR_CONC value 'DD512507025F87F19935000C292086BE'. "#EC NOTEXT
  constants CONTROLLER_CLASS_NOT_FOUND type SOTR_CONC value 'DD512507025F86F19935000C292086BE'. "#EC NOTEXT
  constants MODEL_CLASS_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2DAF19935000C292086BE'. "#EC NOTEXT
  constants PARAMETERS_WAS_NOT_SUPPLIED type SOTR_CONC value 'DD51250CFAA2E0F19935000C292086BE'. "#EC NOTEXT
  constants MODEL_REFERENCE_MISSING type SOTR_CONC value 'DD51250CFAA2DFF19935000C292086BE'. "#EC NOTEXT
  constants MODEL_INSTANCE_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2DEF19935000C292086BE'. "#EC NOTEXT
  constants MODEL_INSTANCE_IS_NULL type SOTR_CONC value 'DD51250CFAA2DCF19935000C292086BE'. "#EC NOTEXT
  constants MODEL_INSTANCE_CREATION_ERROR type SOTR_CONC value 'DD51250CFAA2DBF19935000C292086BE'. "#EC NOTEXT
  constants VIEW_INSTANCE_IS_NULL type SOTR_CONC value 'DD51250CFAA2E3F19935000C292086BE'. "#EC NOTEXT
  constants CONTROLLER_REFERENCE_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2D8F19935000C292086BE'. "#EC NOTEXT
  constants VIEW_INSTANCE_MISSING type SOTR_CONC value 'DD51250CFAA2E4F19935000C292086BE'. "#EC NOTEXT
  constants VIEW_INSTANCE_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2E5F19935000C292086BE'. "#EC NOTEXT
  constants VIEW_REFERENCE_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2E7F19935000C292086BE'. "#EC NOTEXT
  constants CONTROLLER_REFERENCE_MISSING type SOTR_CONC value 'DD512507025F89F19935000C292086BE'. "#EC NOTEXT
  constants VIEW_REFERENCE_MISSING type SOTR_CONC value 'DD51250CFAA2E6F19935000C292086BE'. "#EC NOTEXT
  constants VIEW_CLASS_NOT_FOUND type SOTR_CONC value 'DD51250CFAA2E2F19935000C292086BE'. "#EC NOTEXT

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
*"* protected components of class ZCX_SAPMVC
*"* do not include other source files here!!!

*/=======================================================================\*
*|           _____           _____                                       |*
*|          / ____|   /\    |  __ \  __    __ __     __  _____           |*
*|         | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|          |*
*|          \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |               |*
*|          ____) |/ ____ \ | |     | |\  /| |  \   /  | |____           |*
*|         |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|          |*
*|                                                                       |*
*|  Version 0.1.0                                                        |*
*|                                                                       |*
*\=======================================================================/
*/=======================================================================\
*| SAPmvc is free software: you can redistribute it and/or modify        |*
*| it under the terms of the GNU General Public License as published by  |*
*| the Free Software Foundation, either version 3 of the License, or     |*
*| (at your option) any later version.                                   |*
*|                                                                       |*
*| SAPmvc is distributed in the hope that it will be useful,             |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of        |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         |*
*| GNU General Public License for more details.                          |*
*|                                                                       |*
*| You should have received a copy of the GNU General Public License     |*
*| along with this program. If not, see <http:|www.gnu.org/licenses/>.   |*
*|                                                                       |*
*\=======================================================================/
*/=======================================================================\
*|  Developers:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                    |*
*|                                                                       |*
*|  Tester:                                                              |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                    |*
*|  Changelog:                                                           |*
*|                                                                       |*
*\=======================================================================/

private section.
*"* private components of class ZCX_SAPMVC
*"* do not include other source files here!!!

*/=======================================================================\*
*|           _____           _____                                       |*
*|          / ____|   /\    |  __ \  __    __ __     __  _____           |*
*|         | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|          |*
*|          \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |               |*
*|          ____) |/ ____ \ | |     | |\  /| |  \   /  | |____           |*
*|         |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|          |*
*|                                                                       |*
*|  Version 0.1.0                                                        |*
*|                                                                       |*
*\=======================================================================/
*/=======================================================================\
*| SAPmvc is free software: you can redistribute it and/or modify        |*
*| it under the terms of the GNU General Public License as published by  |*
*| the Free Software Foundation, either version 3 of the License, or     |*
*| (at your option) any later version.                                   |*
*|                                                                       |*
*| SAPmvc is distributed in the hope that it will be useful,             |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of        |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         |*
*| GNU General Public License for more details.                          |*
*|                                                                       |*
*| You should have received a copy of the GNU General Public License     |*
*| along with this program. If not, see <http:|www.gnu.org/licenses/>.   |*
*|                                                                       |*
*\=======================================================================/
*/=======================================================================\
*|  Developers:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                    |*
*|                                                                       |*
*|  Tester:                                                              |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                    |*
*|  Changelog:                                                           |*
*|                                                                       |*
*\=======================================================================/

ENDCLASS.



CLASS ZCX_SAPMVC IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
endmethod.
ENDCLASS.
