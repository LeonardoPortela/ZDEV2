*"* public components of class ZSAPMVC_CONTROLLER
*"* do not include other source files here!!!
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
class ZSAPMVC_CONTROLLER definition
  public
  create public .

public section.

  interfaces ZIF_SAPMVC_CONTROLLER .

  aliases AT_EXIT_COMMAND
    for ZIF_SAPMVC_CONTROLLER~AT_EXIT_COMMAND .
  aliases CALL_SCREEN
    for ZIF_SAPMVC_CONTROLLER~CALL_SCREEN .
  aliases CREATE_MODEL
    for ZIF_SAPMVC_CONTROLLER~CREATE_MODEL .
  aliases PF_STATUS
    for ZIF_SAPMVC_CONTROLLER~PF_STATUS .
  aliases PROCESS_AFTER_INPUT
    for ZIF_SAPMVC_CONTROLLER~PROCESS_AFTER_INPUT .
  aliases PROCESS_BEFORE_OUTPUT
    for ZIF_SAPMVC_CONTROLLER~PROCESS_BEFORE_OUTPUT .
  aliases PROCESS_ON_HELP_REQUEST
    for ZIF_SAPMVC_CONTROLLER~PROCESS_ON_HELP_REQUEST .
  aliases PROCESS_ON_VALUE_REQUEST
    for ZIF_SAPMVC_CONTROLLER~PROCESS_ON_VALUE_REQUEST .
  aliases SET_DYNPRO
    for ZIF_SAPMVC_CONTROLLER~SET_DYNPRO .
  aliases TABSTRIP_ACTIVE_TAB_GET
    for ZIF_SAPMVC_CONTROLLER~TABSTRIP_ACTIVE_TAB_GET .
  aliases TABSTRIP_ACTIVE_TAB_SET
    for ZIF_SAPMVC_CONTROLLER~TABSTRIP_ACTIVE_TAB_SET .
  aliases TITLEBAR
    for ZIF_SAPMVC_CONTROLLER~TITLEBAR .
  aliases USER_COMMAND
    for ZIF_SAPMVC_CONTROLLER~USER_COMMAND .

  types:
    BEGIN OF TYPE_STRUCTURE_CONTROLLER,
        REPID                TYPE SYREPID,
        DYNNR                TYPE SYREPID,
        CONTROLLER_REFERENCE TYPE STRING,
        CONTROLLER_INSTANCE  TYPE REF TO ZIF_SAPMVC_CONTROLLER,
      END OF TYPE_STRUCTURE_CONTROLLER .
  types:
    TYPE_TABLE_CONTROLLER TYPE STANDARD TABLE OF TYPE_STRUCTURE_CONTROLLER .

  constants PROGRAM_ABSOLUTE_NAME type CHAR9 value '\PROGRAM=' ##NO_TEXT.
  constants CLASS_ABSOLUTE_NAME type CHAR7 value '\CLASS=' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
    raising
      ZCX_SAPMVC .
  class-methods CREATE_CONTROLLER
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
      !CONTROLLER_REFERENCE type DATA optional
    exporting
      !VIEW_INSTANCE type ref to ZIF_SAPMVC_VIEW
      value(CONTROLLER_INSTANCE) type ref to ZIF_SAPMVC_CONTROLLER
    raising
      ZCX_SAPMVC .
  class-methods GET_CONTROLLER
    importing
      !REPID type SYREPID optional
      !DYNNR type SYDYNNR optional
      !CONTROLLER_REFERENCE type STRING optional
    preferred parameter CONTROLLER_REFERENCE
    returning
      value(CONTROLLER_INSTANCE) type ref to ZIF_SAPMVC_CONTROLLER
    raising
      ZCX_SAPMVC .
  class-methods SET_CONTROLLER_REFERENCE
    importing
      !REPID type SYREPID
      !DYNNR type SYDYNNR
      !CONTROLLER_REFERENCE type STRING
    raising
      ZCX_SAPMVC .
  class-methods DELETE_CONTROLLER_REFERENCE
    importing
      !REPID type SYREPID optional
      !DYNNR type SYDYNNR optional
      !DELETE_ALL type ABAP_BOOL optional
    preferred parameter DELETE_ALL
    raising
      ZCX_SAPMVC .
protected section.
*"* protected components of class ZSAPMVC_CONTROLLER
*"* do not include other source files here!!!

  aliases CONTROLLER_ERROR
    for ZIF_SAPMVC_CONTROLLER~CONTROLLER_ERROR .
  aliases DYNPRO
    for ZIF_SAPMVC_CONTROLLER~DYNPRO .
  aliases PROGRAM
    for ZIF_SAPMVC_CONTROLLER~PROGRAM .
private section.
*"* private components of class ZSAPMVC_CONTROLLER
*"* do not include other source files here!!!

  class-data TABLE_CONTROLLER type TYPE_TABLE_CONTROLLER .
ENDCLASS.



CLASS ZSAPMVC_CONTROLLER IMPLEMENTATION.


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

  "// Validade Program
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validade Dynpro
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.

  MOVE: repid TO program,
        dynnr TO dynpro.

endmethod.


method create_controller.

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
*|    + Enio Rafael de Jesus                                         |*
*\===================================================================/*

  "// Structure
  data line_controller     type type_structure_controller.
  "// Data Variable
  data controller_ref      type string.
  data absolute_name       type string.
  "// Data Objects
  data create_object_error type ref to cx_sy_create_object_error.
  "// Field-Symbols
  field-symbols: <line_controller>     type type_structure_controller,
                 <line_controller_aux> type type_structure_controller.

  "// Validate Programm
  if ( repid is initial ).
    raise exception type zcx_sapmvc
      exporting
        textid = zcx_sapmvc=>program_missing.
  endif.

  "// Validate Dynpro
  if ( dynnr is initial ).
    raise exception type zcx_sapmvc
      exporting
        textid = zcx_sapmvc=>dynpro_missing.
  endif.

  "// Validate controller Reference
  if ( controller_reference is not supplied ).
*    read table table_controller assigning <line_controller>
*                                 with key repid = repid
*                                          dynnr = dynnr.

    try.
        controller_ref = table_controller[ repid = repid dynnr = dynnr ]-controller_reference.

      catch cx_sy_itab_line_not_found.
        raise exception type zcx_sapmvc
          exporting
            textid = zcx_sapmvc=>controller_reference_not_found.
    endtry.

*    if sy-subrc is initial.
*      move <line_controller>-controller_reference to controller_ref.
*    else.


*    endif.
  else.
    move controller_reference to controller_ref.
  endif.

*  unassign <line_controller>.

  "// Register actual controller Instance
  read table table_controller assigning <line_controller> with key repid = repid
                                                                   dynnr = dynnr.

  if ( sy-subrc is initial ).

    "// Updade Controller Instance
    if controller_ref ne <line_controller>-controller_reference.
      move controller_ref to <line_controller>-controller_reference.
      clear <line_controller>-controller_instance.
    endif.

    if <line_controller>-controller_instance is bound.
      move <line_controller>-controller_instance to controller_instance.
      <line_controller>-controller_instance->set_dynpro( repid = repid dynnr = dynnr ).
    else.

      loop at table_controller assigning <line_controller_aux>
                                     where controller_reference eq controller_ref and
                                           controller_instance  is bound .
        exit.
      endloop.

      if ( sy-subrc is initial ).
        move: <line_controller_aux>-controller_instance to <line_controller>-controller_instance,
              <line_controller_aux>-controller_instance to controller_instance.

        <line_controller>-controller_instance->set_dynpro( repid = repid dynnr = dynnr ).
      else.

        "//Get Local Class Name
        concatenate program_absolute_name repid class_absolute_name controller_ref into absolute_name.

        try.
            "// Create Controller Instance
            create object controller_instance
              type
                (absolute_name)
              exporting
                repid           = repid
                dynnr           = dynnr.

          catch cx_sy_create_object_error.
            "//Get Global Class Name
            concatenate class_absolute_name controller_ref into absolute_name.

            try.
                "// Create Controller Instance
                create object controller_instance
                  type
                    (absolute_name)
                  exporting
                    repid           = repid
                    dynnr           = dynnr.

              catch cx_sy_create_object_error into create_object_error.
                raise exception type zcx_sapmvc
                  exporting
                    textid = create_object_error->textid.
            endtry.
        endtry.

        if sy-subrc is initial.
          move: controller_instance to <line_controller>-controller_instance.

          "// Create Model Instances
          controller_instance->create_model( ).
        endif.
      endif.
    endif.

  else.
    "// Regiter Controller Instance
    loop at table_controller assigning <line_controller_aux>
                                   where controller_reference eq controller_ref and
                                         controller_instance  is bound.
      exit.
    endloop.

    if sy-subrc is initial.

      move: repid                                     to line_controller-repid,
            dynnr                                     to line_controller-dynnr,
            controller_ref                            to line_controller-controller_reference,
            <line_controller_aux>-controller_instance to line_controller-controller_instance,
            <line_controller_aux>-controller_instance to controller_instance.

      line_controller-controller_instance->set_dynpro( repid = repid dynnr = dynnr ).
      append line_controller to table_controller.

    else.
      "//Get Local Class Name
      concatenate program_absolute_name repid class_absolute_name controller_ref into absolute_name.

      try.
          "// Create Controller Instance
          create object controller_instance
            type
              (absolute_name)
            exporting
              repid           = repid
              dynnr           = dynnr.

        catch cx_sy_create_object_error.

          "//Get Global Class Name
          concatenate class_absolute_name controller_ref into absolute_name.

          try.
              "// Create Controller Instance
              create object controller_instance
                type
                  (absolute_name)
                exporting
                  repid           = repid
                  dynnr           = dynnr.

            catch cx_sy_create_object_error into create_object_error.
              raise exception type zcx_sapmvc
                exporting
                  textid = create_object_error->textid.
          endtry.
      endtry.

      if sy-subrc is initial.

        move: repid               to line_controller-repid,
              dynnr               to line_controller-dynnr,
              controller_ref      to line_controller-controller_reference,
              controller_instance to line_controller-controller_instance.

        line_controller-controller_instance->set_dynpro( repid = repid dynnr = dynnr ).
        append line_controller to table_controller.

        "// Create Model Instances
        controller_instance->create_model( ).

      endif.
    endif.
  endif.

  "// View Intance Creation if requested
  if view_instance is requested.

    view_instance = zsapmvc_view=>create_view( repid = repid
                                               dynnr = dynnr
                                               controller = controller_instance ).

  endif.

endmethod.


method DELETE_CONTROLLER_REFERENCE.

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

  IF delete_all IS INITIAL.

    "// Validade Program
    IF repid IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>program_missing.
    ENDIF.
    "// Validade Dynpro
    IF dynnr IS INITIAL.
      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>dynpro_missing.
    ENDIF.

    "// Delelte controller reference for actual program & dynpro
    DELETE table_controller WHERE repid EQ repid AND
                                  dynnr EQ dynnr.

    IF sy-subrc IS INITIAL.
      RETURN.
    ELSE.

      RAISE EXCEPTION TYPE zcx_sapmvc
        EXPORTING
          textid = zcx_sapmvc=>controller_instance_not_found.

    ENDIF.
  ELSE.
    "// Delete all registered controller Instance
    REFRESH table_controller.

  ENDIF.

endmethod.


method GET_CONTROLLER.

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
  DATA line_controller TYPE type_structure_controller.

  "// Validade Controller Reference, Program and Dynpro
  IF controller_reference IS INITIAL AND
     repid                IS INITIAL AND
     dynnr                IS INITIAL.

    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>parameters_was_not_supplied.

  ELSEIF controller_reference IS SUPPLIED.

    "// Get Registered Controller Reference
    READ TABLE table_controller
              INTO line_controller WITH KEY controller_reference = controller_reference.

    "// Validade Program Name and Dynpro Number
  ELSEIF repid IS INITIAL AND
         dynnr IS INITIAL.

    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>parameters_was_not_supplied.

    "// Validade Program
  ELSEIF repid IS INITIAL.

    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.

    "// Validade Dynpro
  ELSEIF dynnr IS INITIAL.

    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.

  ELSE.
    "// Get Registered Controller for Dynpro & Program
    READ TABLE table_controller
              INTO line_controller WITH KEY repid = repid dynnr = dynnr.

  ENDIF.

  "// Validate Controller Instance
  IF line_controller-controller_instance IS BOUND.

    IF dynnr IS NOT INITIAL AND
       repid IS NOT INITIAL.
      "// Set up Program & Dynpro for actual Controller Instance
      line_controller-controller_instance->set_dynpro( repid = repid dynnr = dynnr ).
    ENDIF.

    MOVE line_controller-controller_instance TO controller_instance.

  ELSE.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>controller_instance_not_found.
  ENDIF.

endmethod.


method SET_CONTROLLER_REFERENCE.

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
  DATA line_controller TYPE type_structure_controller.
  "// Field-Symbols
  FIELD-SYMBOLS: <line_controller>     TYPE type_structure_controller,
                 <line_controller_aux> TYPE type_structure_controller.

  "// Valide Program
  IF repid IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>program_missing.
  ENDIF.
  "// Validade Dynpro
  IF dynnr IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>dynpro_missing.
  ENDIF.
  "// Validade Controller Reference
  IF controller_reference IS INITIAL.
    RAISE EXCEPTION TYPE zcx_sapmvc
      EXPORTING
        textid = zcx_sapmvc=>controller_reference_missing.
  ENDIF.

  "// Get Registered Controller Reference for actual program & dynpro
  READ TABLE table_controller ASSIGNING <line_controller>
                               WITH KEY repid                = repid
                                        dynnr                = dynnr.

  "// Controller reference already registered
  IF sy-subrc IS INITIAL.
    IF controller_reference EQ <line_controller>-controller_reference.
      RETURN.
    ELSE.

      "// Get Registered Controller Reference for controller actual reference name
      READ TABLE table_controller ASSIGNING <line_controller_aux>
                                   WITH KEY controller_reference = controller_reference.

      IF sy-subrc IS INITIAL.
        MOVE: <line_controller_aux>-controller_instance TO <line_controller>-controller_instance.
      ELSE.
        FREE <line_controller>-controller_instance.
        MOVE: controller_reference TO <line_controller>-controller_reference.
      ENDIF.
    ENDIF.
  ELSE.
    "// Register a new reference
    MOVE: repid                TO line_controller-repid,
          dynnr                TO line_controller-dynnr,
          controller_reference TO line_controller-controller_reference.

    APPEND line_controller TO table_controller.

  ENDIF.

endmethod.


method ZIF_SAPMVC_CONTROLLER~AT_EXIT_COMMAND.

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


method ZIF_SAPMVC_CONTROLLER~CALL_SCREEN.

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


method ZIF_SAPMVC_CONTROLLER~CREATE_MODEL.

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


method ZIF_SAPMVC_CONTROLLER~PF_STATUS.

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


method ZIF_SAPMVC_CONTROLLER~PROCESS_AFTER_INPUT.

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


method ZIF_SAPMVC_CONTROLLER~PROCESS_BEFORE_OUTPUT.

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


method ZIF_SAPMVC_CONTROLLER~PROCESS_ON_HELP_REQUEST.

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


method ZIF_SAPMVC_CONTROLLER~PROCESS_ON_VALUE_REQUEST.

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


method ZIF_SAPMVC_CONTROLLER~SET_DYNPRO.

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

  MOVE: repid TO program,
        dynnr TO dynpro.

endmethod.


method ZIF_SAPMVC_CONTROLLER~TABSTRIP_ACTIVE_TAB_GET.

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


method ZIF_SAPMVC_CONTROLLER~TABSTRIP_ACTIVE_TAB_SET.

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


method ZIF_SAPMVC_CONTROLLER~TITLEBAR.

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


method ZIF_SAPMVC_CONTROLLER~USER_COMMAND.

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
ENDCLASS.
