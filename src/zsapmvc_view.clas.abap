class ZSAPMVC_VIEW definition
  public
  create public .

*"* public components of class ZSAPMVC_VIEW
*"* do not include other source files here!!!
public section.

  interfaces ZIF_SAPMVC_VIEW .

  aliases CHANGE_NOTIFICATION
    for ZIF_SAPMVC_VIEW~CHANGE_NOTIFICATION .
  aliases TRIGGER_AT_EXIT_COMMAND
    for ZIF_SAPMVC_VIEW~TRIGGER_AT_EXIT_COMMAND .
  aliases TRIGGER_PAI
    for ZIF_SAPMVC_VIEW~TRIGGER_PAI .
  aliases TRIGGER_PBO
    for ZIF_SAPMVC_VIEW~TRIGGER_PBO .
  aliases TRIGGER_POH
    for ZIF_SAPMVC_VIEW~TRIGGER_POH .
  aliases TRIGGER_POV
    for ZIF_SAPMVC_VIEW~TRIGGER_POV .
  aliases TRIGGER_STATUS
    for ZIF_SAPMVC_VIEW~TRIGGER_STATUS .
  aliases TRIGGER_TABSTRIP_ACTIVE_GET
    for ZIF_SAPMVC_VIEW~TRIGGER_TABSTRIP_ACTIVE_GET .
  aliases TRIGGER_TABSTRIP_ACTIVE_SET
    for ZIF_SAPMVC_VIEW~TRIGGER_TABSTRIP_ACTIVE_SET .
  aliases TRIGGER_TITLEBAR
    for ZIF_SAPMVC_VIEW~TRIGGER_TITLEBAR .
  aliases TRIGGER_USER_COMMAND
    for ZIF_SAPMVC_VIEW~TRIGGER_USER_COMMAND .
  aliases AT_EXIT_COMMAND
    for ZIF_SAPMVC_VIEW~AT_EXIT_COMMAND .
  aliases PF_STATUS
    for ZIF_SAPMVC_VIEW~PF_STATUS .
  aliases PROCESS_AFTER_INPUT
    for ZIF_SAPMVC_VIEW~PROCESS_AFTER_INPUT .
  aliases PROCESS_BEFORE_OUTPUT
    for ZIF_SAPMVC_VIEW~PROCESS_BEFORE_OUTPUT .
  aliases PROCESS_ON_HELP_REQUEST
    for ZIF_SAPMVC_VIEW~PROCESS_ON_HELP_REQUEST .
  aliases PROCESS_ON_VALUE_REQUEST
    for ZIF_SAPMVC_VIEW~PROCESS_ON_VALUE_REQUEST .
  aliases TABSTRIP_ACTIVE_TAB_GET
    for ZIF_SAPMVC_VIEW~TABSTRIP_ACTIVE_TAB_GET .
  aliases TABSTRIP_ACTIVE_TAB_SET
    for ZIF_SAPMVC_VIEW~TABSTRIP_ACTIVE_TAB_SET .
  aliases TITLEBAR
    for ZIF_SAPMVC_VIEW~TITLEBAR .
  aliases USER_COMMAND
    for ZIF_SAPMVC_VIEW~USER_COMMAND .

  types:
    BEGIN OF type_structure_view,
              repid TYPE syrepid,
              dynnr TYPE sydynnr,
              view_reference TYPE string,
              view_instance TYPE REF TO zif_sapmvc_view,
            END OF type_structure_view .
  types:
    type_table_view TYPE STANDARD TABLE OF type_structure_view .

  methods CONSTRUCTOR
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
      !CONTROLLER type ref to ZIF_SAPMVC_CONTROLLER
    raising
      ZCX_SAPMVC .
  class-methods CREATE_VIEW
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
      !CONTROLLER type ref to ZIF_SAPMVC_CONTROLLER
      !VIEW_REFERENCE type STRING optional
    returning
      value(VIEW_INSTANCE) type ref to ZIF_SAPMVC_VIEW
    raising
      ZCX_SAPMVC .
  class-methods GET_VIEW
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
    returning
      value(VIEW_INSTANCE) type ref to ZIF_SAPMVC_VIEW
    raising
      ZCX_SAPMVC .
  type-pools ABAP .
  class-methods ACTIVATE_CHANGE_NOTIFICATION
    importing
      !MODEL_INSTANCE type ref to ZSAPMVC_MODEL
      !VIEW_INSTANCE type ref to ZIF_SAPMVC_VIEW
      !TIMEOUT type I optional
      !REPEAT type ABAP_BOOL optional
    raising
      ZCX_SAPMVC .
  class-methods DEACTIVATE_CHANGE_NOTIFICATION
    importing
      !MODEL_INSTANCE type ref to ZSAPMVC_MODEL optional
      !VIEW_INSTANCE type ref to ZIF_SAPMVC_VIEW
    preferred parameter MODEL_INSTANCE
    raising
      ZCX_SAPMVC .
  class-methods SET_VIEW_REFERENCE
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
      !VIEW_REFERENCE type STRING
    raising
      ZCX_SAPMVC .
  class-methods DELETE_VIEW_REFERENCE
    importing
      !REPID type SYREPID optional
      !DYNNR type SYDYNNR optional
      !DELETE_ALL type ABAP_BOOL optional
    raising
      ZCX_SAPMVC .
protected section.
*"* protected components of class ZSAPMVC_VIEW
*"* do not include other source files here!!!

  constants VIEW_CLASS type STRING value 'ZSAPMVC_VIEW'. "#EC NOTEXT
  constants PROGRAM_ABSOLUTE_NAME type CHAR9 value '\PROGRAM='. "#EC NOTEXT
  constants CLASS_ABSOLUTE_NAME type CHAR7 value '\CLASS='. "#EC NOTEXT
  data PROGRAM type SYREPID .
  data DYNPRO type SYDYNNR .
private section.
*"* private components of class ZSAPMVC_VIEW
*"* do not include other source files here!!!

  class-data TABLE_VIEW type TYPE_TABLE_VIEW .
ENDCLASS.



CLASS ZSAPMVC_VIEW IMPLEMENTATION.


method ACTIVATE_CHANGE_NOTIFICATION.

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

  "// validade View Instance
  IF view_instance IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>view_instance_is_null.
  ENDIF.

  "// Validade Model Instance
  IF model_instance IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>model_instance_is_null.

  ELSE.
    "// Activate Change Noticication for Model Instance
    SET HANDLER view_instance->change_notification FOR model_instance.

    "// Set up the Notification Time
    model_instance->start_timer( timeout = timeout
                                 repeat  = repeat  ).

  ENDIF.

endmethod.


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

  "// Validade Program Name
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validade Dynpro Number
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.
  "// Validade Controller Instance
  IF controller IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>controller_instance_missing.
  ENDIF.

  MOVE repid TO me->program.
  MOVE dynnr TO me->dynpro.

  "// Register all Event Handlers for View Events
  SET HANDLER controller->user_command
              controller->process_after_input
              controller->process_before_output
              controller->pf_status
              controller->titlebar
              controller->process_on_value_request
              controller->process_on_help_request
              controller->tabstrip_active_tab_get
              controller->tabstrip_active_tab_set
              controller->at_exit_command FOR me.

endmethod.


method CREATE_VIEW.

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

  "// Structure
  DATA line_view     TYPE type_structure_view.
  "// Data Variable
  DATA absolute_name TYPE string.
  DATA view_ref      TYPE string.
  "// Data Objects
  DATA create_object_error TYPE REF TO cx_sy_create_object_error.
  "// Field-Symbols
  FIELD-SYMBOLS <line_view>     TYPE type_structure_view.

  "// Validate Programm Name
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validate Dynpro Number
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.
  "// Validate Controller Instance
  IF controller IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>controller_instance_missing.
  ENDIF.

  "// Validate view Reference
  IF view_reference IS INITIAL.

    "// Register actual View Instance
    READ TABLE table_view ASSIGNING <line_view> WITH KEY repid = repid
                                                         dynnr = dynnr.

    IF sy-subrc IS INITIAL.
      MOVE <line_view>-view_reference TO view_ref.
    ELSE.
      MOVE zsapmvc_view=>view_class TO view_ref.
    ENDIF.

  ELSE.
    MOVE view_reference TO view_ref.
  ENDIF.

  UNASSIGN <line_view>.

  "// Register actual View Instance
  READ TABLE table_view ASSIGNING <line_view> WITH KEY repid = repid
                                                       dynnr = dynnr.

  IF sy-subrc IS INITIAL.

    "// Update view Instance
    IF view_ref NE <line_view>-view_reference.
      MOVE view_ref TO <line_view>-view_reference.
      CLEAR <line_view>-view_instance.
    ENDIF.

    "// Reuse view Instance
    IF <line_view>-view_instance IS BOUND.
      MOVE <line_view>-view_instance TO view_instance.
    ELSE.

      "// Get Global Class Name
      CONCATENATE class_absolute_name view_ref INTO absolute_name.

      TRY.
          CREATE OBJECT view_instance
            TYPE
              (absolute_name)
            EXPORTING
              repid           = repid
              dynnr           = dynnr
              controller      = controller.

        CATCH cx_sy_create_object_error.

          "//Get Local Class Name
          CONCATENATE program_absolute_name repid class_absolute_name view_ref INTO absolute_name.

          TRY.
              "// Create Controller Instance
              CREATE OBJECT view_instance
                TYPE
                  (absolute_name)
                EXPORTING
                  repid           = repid
                  dynnr           = dynnr
                  controller      = controller.

            CATCH cx_sy_create_object_error INTO create_object_error.
              RAISE EXCEPTION TYPE zcx_sapmvc
                           EXPORTING
                             textid = create_object_error->textid.
          ENDTRY.

      ENDTRY.

      IF sy-subrc IS INITIAL.
        MOVE: view_instance TO <line_view>-view_instance.
      ENDIF.

    ENDIF.

    "// Create and Register a new View Instance
  ELSE.

    "//Get Global Class Name
    CONCATENATE class_absolute_name view_ref INTO absolute_name.

    TRY.
        CREATE OBJECT view_instance
          TYPE
            (absolute_name)
          EXPORTING
            repid           = repid
            dynnr           = dynnr
            controller      = controller.

      CATCH cx_sy_create_object_error.

        "//Get Global Class Name
        CONCATENATE program_absolute_name repid class_absolute_name view_ref INTO absolute_name.

        TRY.
            "// Create Controller Instance
            CREATE OBJECT view_instance
              TYPE
                (absolute_name)
              EXPORTING
                repid           = repid
                dynnr           = dynnr
                controller      = controller.

          CATCH cx_sy_create_object_error INTO create_object_error.
            RAISE EXCEPTION TYPE zcx_sapmvc
                         EXPORTING
                           textid = create_object_error->textid.
        ENDTRY.

    ENDTRY.

    IF sy-subrc IS INITIAL.
      "// Register a new View Instance
      MOVE: view_instance TO line_view-view_instance,
            view_ref      TO line_view-view_reference,
            repid         TO line_view-repid,
            dynnr         TO line_view-dynnr.

      APPEND line_view TO table_view.
      CLEAR line_view.

    ENDIF.
  ENDIF.

endmethod.


method DEACTIVATE_CHANGE_NOTIFICATION.

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

  "// Validade View Instance
  IF view_instance IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>view_instance_is_null.
  ENDIF.

  "// Validade Model Instance
  IF model_instance IS NOT BOUND.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>model_instance_is_null.

  ELSE.
    "// Deactivate Notification for Model Instance
    SET HANDLER view_instance->change_notification FOR model_instance ACTIVATION space.

    model_instance->end_timer( ).

  ENDIF.

endmethod.


method DELETE_VIEW_REFERENCE.

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

  IF delete_all IS  INITIAL.

    "// Validade Program Name
    IF repid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>program_missing.
    ENDIF.
    "// Validade Dynpro Number
    IF dynnr IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>dynpro_missing.
    ENDIF.
    "// Delte the View Reference
    DELETE table_view WHERE repid EQ repid AND
                            dynnr EQ dynnr.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>view_instance_not_found.
    ENDIF.

  ELSE.
    "// Delete all registered view Instance
    REFRESH table_view.

  ENDIF.

endmethod.


method GET_VIEW.

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

  "// Structure
  DATA line_view TYPE type_structure_view.

  "// Validade Program Name
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validade Dynpro Number
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.

  "// Get registered View Instance
  READ TABLE table_view INTO line_view WITH KEY repid = repid
                                                dynnr = dynnr.

  IF line_view-view_instance IS BOUND.
    MOVE: line_view-view_instance TO view_instance.
  ELSE.

    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>view_instance_not_found.

  ENDIF.

endmethod.


method SET_VIEW_REFERENCE.

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

  "// Structure
  DATA line_view TYPE type_structure_view.
  "// Field-Symbols
  FIELD-SYMBOLS: <line_view>     TYPE type_structure_view,
                 <line_view_aux> TYPE type_structure_view.

  "// Validade Program Name
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validade Dynpro Number
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.
  "// Validade View Instance
  IF view_reference IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>view_reference_missing.
  ENDIF.

  "// Get Registered View
  READ TABLE table_view ASSIGNING <line_view> WITH KEY repid = repid
                                                       dynnr = dynnr.

  IF sy-subrc IS INITIAL.
    IF view_reference EQ <line_view>-view_reference.
      RETURN.
    ELSE.
      "// Update View Reference
      READ TABLE table_view ASSIGNING <line_view_aux>
                                   WITH KEY view_reference = view_reference.

      IF sy-subrc IS INITIAL.
        MOVE: <line_view_aux>-view_instance TO <line_view>-view_instance.
      ELSE.
        FREE <line_view>-view_instance.
        MOVE: view_reference TO <line_view>-view_reference.
      ENDIF.
    ENDIF.
  ELSE.
    "// Regsiter a new View Reference
    MOVE: repid                TO line_view-repid,
          dynnr                TO line_view-dynnr,
          view_reference TO line_view-view_reference.

    APPEND line_view TO table_view.

  ENDIF.

endmethod.


method ZIF_SAPMVC_VIEW~CHANGE_NOTIFICATION.

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

*  The implementation must be made in subclass method if necessary !
endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_AT_EXIT_COMMAND.

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

  RAISE EVENT at_exit_command EXPORTING ucomm =  ucomm.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_PAI.

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

  RAISE EVENT process_after_input EXPORTING repid = program
                                            dynnr = dynpro.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_PBO.

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

  RAISE EVENT process_before_output EXPORTING repid = program
                                              dynnr = dynpro.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_POH.

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

  RAISE EVENT process_on_help_request EXPORTING field = field.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_POV.

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

  RAISE EVENT process_on_value_request EXPORTING field = field.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_STATUS.

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

  RAISE EVENT pf_status EXPORTING repid = program
                                  dynnr = dynpro.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_TABSTRIP_ACTIVE_GET.

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

  RAISE EVENT tabstrip_active_tab_get EXPORTING  ucomm = ucomm.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_TABSTRIP_ACTIVE_SET.

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

  RAISE EVENT tabstrip_active_tab_set EXPORTING ucomm = ucomm.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_TITLEBAR.

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

  RAISE EVENT titlebar EXPORTING repid = program
                                 dynnr = dynpro.

endmethod.


method ZIF_SAPMVC_VIEW~TRIGGER_USER_COMMAND.

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

  RAISE EVENT user_command EXPORTING ucomm =  ucomm.

endmethod.
ENDCLASS.
