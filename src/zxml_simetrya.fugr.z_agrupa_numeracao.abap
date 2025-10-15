FUNCTION z_agrupa_numeracao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(TXT_AGRUPA) TYPE  STRING
*"  TABLES
*"      IT_NUMEROS STRUCTURE  ZTNUMERONFE
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_numero.
  TYPES:   numero_n TYPE n LENGTH 9,
           numero_i TYPE i.
  TYPES: END OF ty_numero.

  TYPES: BEGIN OF ty_numero_seq.
  TYPES:   numero_i TYPE i,
           numero_f TYPE i.
  TYPES: END OF ty_numero_seq.

  DATA: it_numero   TYPE TABLE OF ty_numero INITIAL SIZE 0 WITH HEADER LINE,
        it_sequenc  TYPE TABLE OF ty_numero_seq INITIAL SIZE 0 WITH HEADER LINE,
        wa_numero   TYPE ty_numero,
        wa_numeros  TYPE ztnumeronfe,
        wa_sequenc  TYPE ty_numero_seq,
        vg_tabix    TYPE sy-tabix,
        numero_ante TYPE i,
        numero_prox TYPE i,
        vg_agrupa   TYPE string,
        vg_agrupa_i TYPE c LENGTH 9,
        vg_agrupa_f TYPE c LENGTH 9.

  CLEAR: txt_agrupa.

  LOOP AT it_numeros INTO wa_numeros.
    CLEAR: wa_numero.
    WRITE wa_numeros-nfenum TO wa_numero-numero_n.
    wa_numero-numero_i = wa_numero-numero_n.
    APPEND wa_numero TO it_numero.
  ENDLOOP.

  SORT it_numero BY numero_i.

  LOOP AT it_numero INTO wa_numero.
    vg_tabix = 0.

    LOOP AT it_sequenc INTO wa_sequenc.
      vg_tabix = sy-tabix.
      IF wa_numero-numero_i GT wa_sequenc-numero_i.
        IF NOT wa_sequenc-numero_f IS INITIAL.
          IF wa_numero-numero_i LE wa_sequenc-numero_f.
            vg_tabix = 9.
          ELSE.
            numero_prox = wa_sequenc-numero_f + 1.
            IF wa_numero-numero_i EQ numero_prox.
              wa_sequenc-numero_f = wa_numero-numero_i.
              MODIFY it_sequenc INDEX vg_tabix FROM wa_sequenc TRANSPORTING numero_f.
              vg_tabix = 9.
            ELSE.
              vg_tabix = 0.
            ENDIF.
          ENDIF.
        ELSE.
          numero_ante = wa_numero-numero_i - 1.
          IF numero_ante EQ wa_sequenc-numero_i.
            wa_sequenc-numero_f = wa_numero-numero_i.
            MODIFY it_sequenc INDEX vg_tabix FROM wa_sequenc TRANSPORTING numero_f.
            vg_tabix = 9.
          ELSE.
            vg_tabix = 0.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF vg_tabix EQ 0.
      wa_sequenc-numero_i = wa_numero-numero_i.
      CLEAR: wa_sequenc-numero_f.
      APPEND wa_sequenc TO it_sequenc.
    ENDIF.

  ENDLOOP.

  LOOP AT it_sequenc INTO wa_sequenc.

    CLEAR: vg_agrupa.

    IF NOT wa_sequenc-numero_f IS INITIAL.
      WRITE wa_sequenc-numero_i TO vg_agrupa_i.
      WRITE wa_sequenc-numero_f TO vg_agrupa_f.
      SHIFT vg_agrupa_i LEFT DELETING LEADING space.
      SHIFT vg_agrupa_f LEFT DELETING LEADING space.
      CONCATENATE vg_agrupa_i 'a' vg_agrupa_f INTO vg_agrupa SEPARATED BY space.
    ELSE.
      WRITE wa_sequenc-numero_i TO vg_agrupa_i.
      SHIFT vg_agrupa_i LEFT DELETING LEADING space.
      MOVE vg_agrupa_i TO vg_agrupa.
    ENDIF.

    IF txt_agrupa IS INITIAL.
      txt_agrupa = vg_agrupa.
    ELSE.
      CONCATENATE txt_agrupa ',' INTO txt_agrupa.
      CONCATENATE txt_agrupa vg_agrupa INTO txt_agrupa SEPARATED BY space.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
