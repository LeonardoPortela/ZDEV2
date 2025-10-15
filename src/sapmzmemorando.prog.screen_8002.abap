
PROCESS BEFORE OUTPUT.

  MODULE tab_cabe_protoc_change_tc_attr.
  LOOP AT   it_protocolos
       WITH CONTROL tab_cabe_protoc
       CURSOR tab_cabe_protoc-current_line.
  ENDLOOP.

* MODULE STATUS_8002.
*
PROCESS AFTER INPUT.

  LOOP AT it_protocolos.
    FIELD it_protocolos-mark
      MODULE tab_cabe_protoc_mark ON REQUEST.
  ENDLOOP.

* MODULE USER_COMMAND_8002.
