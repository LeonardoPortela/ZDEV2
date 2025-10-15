class ZSAPMVC_MODEL definition
  public
  create public .

*"* public components of class ZSAPMVC_MODEL
*"* do not include other source files here!!!
public section.

  types:
    type_table_fieldname TYPE TABLE OF fieldname .

  events CHANGE_NOTIFICATION
    exporting
      value(FIELD) type FIELDNAME optional .

  methods CONSTRUCTOR .
  class-methods CREATE_MODEL
    importing
      !MODEL_REFERENCE type STRING
      !REPID type SYREPID
    returning
      value(MODEL_INSTANCE) type ref to ZSAPMVC_MODEL
    raising
      ZCX_SAPMVC .
  type-pools ABAP .
  methods START_TIMER
  final
    importing
      !TIMEOUT type I optional
      !REPEAT type ABAP_BOOL optional .
  methods END_TIMER
  final .
protected section.
*"* protected components of class ZSAPMVC_MODEL
*"* do not include other source files here!!!

  constants PROGRAM_ABSOLUTE_NAME type CHAR9 value '\PROGRAM=' ##NO_TEXT.
  constants CLASS_ABSOLUTE_NAME type CHAR7 value '\CLASS=' ##NO_TEXT.
  data TIMER type ref to CL_GUI_TIMER .
  data REPEAT_TIMER type ABAP_BOOL .

  methods TRIGGER_CHANGE_NOTIFICATION
  final .
  methods RUN_TIMER
  final
    for event FINISHED of CL_GUI_TIMER .
  methods VIEW_NOTIFICATION
    returning
      value(NOTIFY_VIEW) type ABAP_BOOL .
private section.
*"* private components of class ZSAPMVC_MODEL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZSAPMVC_MODEL IMPLEMENTATION.


method CONSTRUCTOR.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "// Create Timer Instance
  CREATE OBJECT timer.
  "// Register the Event timer for timer Events
  SET HANDLER me->run_timer FOR timer.

endmethod.


method CREATE_MODEL.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "// Data Variable
  DATA absolute_name TYPE string.

  "// Validate Programm Name
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.

  "// Validate Model Class Reference
  IF model_reference IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>model_instance_not_found.
  ENDIF.

  "// Get Model Class Name
  CONCATENATE program_absolute_name repid class_absolute_name model_reference INTO absolute_name.

  CREATE OBJECT model_instance
    TYPE
      (absolute_name).

  IF sy-subrc IS NOT INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>model_instance_creation_error.

  ENDIF.

endmethod.


method END_TIMER.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "// End repeat notification
  MOVE space TO repeat_timer.

endmethod.


method RUN_TIMER.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "// While timer is active and NOTIFY_VIEW isn't filled Run Notification Timer
  IF me->view_notification( ) IS INITIAL.
    timer->run( ).
  ELSE.
    "// When NOTIFY_VIEW is filled Trigger Notification View Event
    me->trigger_change_notification( ).
    IF repeat_timer EQ 'X'.
      timer->run( ).
    ENDIF.
  ENDIF.

endmethod.


method START_TIMER.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "// Set up the notification timer (Default = 5 sec)
  IF timeout IS INITIAL.
    timer->interval = 5.
  ELSE.
    timer->interval = timeout.
  ENDIF.

  "// Set up Notification Repeat
  MOVE repeat TO repeat_timer.

  "// Run Notification timer
  timer->run( ).

endmethod.


method TRIGGER_CHANGE_NOTIFICATION.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  RAISE EVENT change_notification.

endmethod.


method VIEW_NOTIFICATION.

*/===================================================================\*
*|         _____           _____                                     |*
*|        / ____|   /\    |  __ \  __    __ __     __  _____         |*
*|       | (___    /  \   | |__) ||  \  /  |\ \   / / / ____|        |*
*|        \___ \  / /\ \  |  ___/ |   \/   | \ \ / / | |             |*
*|        ____) |/ ____ \ | |     | |\  /| |  \   /  | |____         |*
*|       |_____//_/    \_\|_|     |_| \/ |_|   \_/    \ ____|        |*
*|                                                                   |*
*|  Version 0.1.0                                                    |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*| SAPmvc is free software: you can redistribute it and/or modify    |*
*| it under the terms of the GNU General Public License as published |*
*| by the Free Software Foundation, either version 3 of the License, |*
*| or (at your option) any later version.                            |*
*|                                                                   |*
*| SAPmvc is distributed in the hope that it will be useful,         |*
*| but WITHOUT ANY WARRANTY; without even the implied warranty of    |*
*| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |*
*| GNU General Public License for more details.                      |*
*|                                                                   |*
*| You should have received a copy of the GNU General Public License |*
*| along with this program. If not, see http://www.gnu.org/licenses. |*
*|                                                                   |*
*\===================================================================/*
*/===================================================================\*
*|  Developers:                                                      |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|                                                                   |*
*|  Tester:                                                          |*
*|    + Marcelo Araujo Ramos ( mepmarcelo@gmail.com )                |*
*|  Changelog:                                                       |*
*|                                                                   |*
*\===================================================================/*

  "//  The implementation must be made in subclass method if necessary !

  "// Insert here the condition to process timer
  "// To notify view set 'X' to return parameter NOTIFY_VIEW.
  "// Sample:

  "//     IF field1 NE field2.
  "//       MOVE 'X' TO NOTIFY_VIEW.
  "//     ENDIF.

endmethod.
ENDCLASS.
