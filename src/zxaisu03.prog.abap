*&---------------------------------------------------------------------*
*&  Include           ZXAISU03
*&---------------------------------------------------------------------*

TYPES: BEGIN OF type_tvarvc,
         name TYPE tvarvc-name,
         numb TYPE tvarvc-numb,
         low  TYPE tvarvc-low,
         high TYPE tvarvc-high,
       END   OF type_tvarvc.

DATA: tl_tvarvc   TYPE TABLE OF type_tvarvc,
      tl_fator    TYPE TABLE OF type_tvarvc,
      tl_cpc27    TYPE TABLE OF type_tvarvc,
      tl_anel     TYPE TABLE OF type_tvarvc,
      tl_vuanel   TYPE TABLE OF type_tvarvc,
      tl_vuanel_obras   TYPE TABLE OF type_tvarvc,
      tl_vucpc27  TYPE TABLE OF type_tvarvc,
      sl_tvarvc   TYPE type_tvarvc         ,
      sl_aux      TYPE type_tvarvc         ,
      sl_vuanel_obras TYPE type_tvarvc     ,
      sl_anlb     TYPE anlb                ,
      vl_cpc27    TYPE tvarvc-name         ,
      vl_anel     TYPE tvarvc-name         ,
      vl_fator    TYPE tvarvc-name         ,
      vl_vuanel   TYPE tvarvc-name         ,
      vl_vuanel_obras TYPE tvarvc-name     ,
      vl_vucpc27  TYPE tvarvc-name         ,
      vl_anlkl    TYPE anla-anlkl          ,
      vl_ndjar    TYPE anlb-ndjar          ,
      vl_esquerda TYPE char4               ,
      vl_direita  TYPE char4               ,
      vl_index    TYPE i                   ,
      vl_campo    TYPE char20 VALUE '(SAPLAIST)ANLZ-MSFAK'.

FIELD-SYMBOLS <msfak> TYPE anlz-msfak.

REFRESH tl_tvarvc.
CHECK sy-tcode EQ 'AS01'.

vl_cpc27   = 'EMPRESAS_OUT_CPC27'.
vl_anel    = 'EMPRESAS_ANEL'.
vl_fator   = 'FATOR_TURNO'.
vl_vuanel  = 'VU_ANEL'.
vl_vuanel_obras = 'VU_ANEL_OBRAS'.
vl_vucpc27 = 'VU_OUT_CPC27'.

* Seleciona TVARVC
SELECT name numb low high
  FROM tvarvc
  INTO TABLE tl_tvarvc.

CHECK NOT tl_tvarvc[] IS INITIAL.
SORT tl_tvarvc BY low ASCENDING.

tl_fator[] = tl_tvarvc[].
DELETE tl_fator WHERE name NE vl_fator.

LOOP AT tl_fator INTO sl_tvarvc.
  vl_index = sy-tabix.
  vl_anlkl = sl_tvarvc-low.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_anlkl
    IMPORTING
      output = vl_anlkl.
  sl_tvarvc-low = vl_anlkl.
  MODIFY tl_fator FROM sl_tvarvc
    INDEX vl_index
    TRANSPORTING low.
  CLEAR sl_tvarvc.
ENDLOOP.

*Anel Exceções
tl_vuanel_obras[] = tl_tvarvc[].
DELETE: tl_vuanel_obras WHERE name NE vl_vuanel_obras.

LOOP AT tl_vuanel_obras INTO sl_tvarvc.
  vl_index = sy-tabix.
  vl_anlkl = sl_tvarvc-low.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_anlkl
    IMPORTING
      output = vl_anlkl.
  sl_tvarvc-low = vl_anlkl.

  MODIFY tl_vuanel_obras FROM sl_tvarvc INDEX vl_index TRANSPORTING low.
  CLEAR: sl_tvarvc.
ENDLOOP.

* CPC27
tl_cpc27[]   = tl_tvarvc[].
tl_vucpc27[] = tl_tvarvc[].
DELETE: tl_cpc27   WHERE name NE vl_cpc27  ,
        tl_vucpc27 WHERE name NE vl_vucpc27.

IF NOT tl_cpc27[] IS INITIAL.

  LOOP AT tl_vucpc27 INTO sl_tvarvc.
    vl_index = sy-tabix.
    vl_anlkl = sl_tvarvc-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vl_anlkl
      IMPORTING
        output = vl_anlkl.
    sl_tvarvc-low = vl_anlkl.
    SPLIT sl_tvarvc-high AT ','
      INTO vl_esquerda vl_direita.
    sl_tvarvc-high = vl_esquerda.
    MODIFY tl_vucpc27 FROM sl_tvarvc
      INDEX vl_index
      TRANSPORTING low high.
    CLEAR: sl_tvarvc  ,
           vl_esquerda,
           vl_direita .
  ENDLOOP.

  READ TABLE tl_cpc27 INTO sl_tvarvc
    WITH KEY low = i_anla-bukrs
    BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    CLEAR: sl_vuanel_obras.
    READ TABLE tl_vuanel_obras INTO sl_vuanel_obras
      WITH KEY low = i_anla-anlkl.

    READ TABLE tl_vucpc27 INTO sl_aux
      WITH KEY low = i_anla-anlkl
      BINARY SEARCH.

    READ TABLE t_anlb INTO sl_anlb
      WITH KEY afabe = 1.

    IF sy-subrc IS INITIAL.
      vl_index = sy-tabix.
      sl_anlb-afasl = sl_tvarvc-high.
      sl_anlb-ndjar = sl_aux-high.

      if sl_vuanel_obras is NOT INITIAL.
        sl_anlb-afasl = sl_vuanel_obras-high.
      endif.

      MODIFY t_anlb FROM sl_anlb
        INDEX vl_index
        TRANSPORTING afasl ndjar.
    ENDIF.

    READ TABLE t_anlb INTO sl_anlb
      WITH KEY afabe = 5.

    IF sy-subrc IS INITIAL.
      vl_index = sy-tabix.
      sl_anlb-afasl = sl_tvarvc-high.
      sl_anlb-ndjar = sl_aux-high.

      if sl_vuanel_obras is NOT INITIAL.
        sl_anlb-afasl = sl_vuanel_obras-high.
      endif.

      MODIFY t_anlb FROM sl_anlb
        INDEX vl_index
        TRANSPORTING afasl ndjar.
    ENDIF.

  ENDIF.

ENDIF.

* Anel
tl_anel[]   = tl_tvarvc[].
tl_vuanel[] = tl_tvarvc[].
DELETE: tl_anel   WHERE name NE vl_anel  ,
        tl_vuanel WHERE name NE vl_vuanel.

IF NOT tl_anel[] IS INITIAL.

  LOOP AT tl_vuanel INTO sl_tvarvc.
    vl_index = sy-tabix.
    vl_anlkl = sl_tvarvc-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vl_anlkl
      IMPORTING
        output = vl_anlkl.
    sl_tvarvc-low = vl_anlkl.
    SPLIT sl_tvarvc-high AT ','
      INTO vl_esquerda vl_direita.
    sl_tvarvc-high = vl_esquerda.
    MODIFY tl_vuanel FROM sl_tvarvc
      INDEX vl_index
      TRANSPORTING low high.
    CLEAR: sl_tvarvc  ,
           vl_esquerda,
           vl_direita .
  ENDLOOP.

  READ TABLE tl_anel INTO sl_tvarvc
    WITH KEY low = i_anla-bukrs
    BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    CLEAR: sl_vuanel_obras.
    READ TABLE tl_vuanel_obras INTO sl_vuanel_obras
      WITH KEY low = i_anla-anlkl.

    READ TABLE tl_vuanel INTO sl_aux
      WITH KEY low = i_anla-anlkl
      BINARY SEARCH.

    READ TABLE t_anlb INTO sl_anlb
      WITH KEY afabe = 1.

    IF sy-subrc IS INITIAL.
      vl_index = sy-tabix.
      sl_anlb-afasl = sl_tvarvc-high.
      sl_anlb-ndjar = sl_aux-high.

      if sl_vuanel_obras is NOT INITIAL.
        sl_anlb-afasl = sl_vuanel_obras-high.
      endif.

      MODIFY t_anlb FROM sl_anlb
        INDEX vl_index
        TRANSPORTING afasl ndjar.
    ENDIF.

    READ TABLE t_anlb INTO sl_anlb
      WITH KEY afabe = 5.

    IF sy-subrc IS INITIAL.
      vl_index = sy-tabix.
      sl_anlb-afasl = sl_tvarvc-high.
      sl_anlb-ndjar = sl_aux-high.

      if sl_vuanel_obras is NOT INITIAL.
        sl_anlb-afasl = sl_vuanel_obras-high.
      endif.

      MODIFY t_anlb FROM sl_anlb
        INDEX vl_index
        TRANSPORTING afasl ndjar.
    ENDIF.

  ENDIF.

ENDIF.

READ TABLE tl_fator INTO sl_tvarvc
  WITH KEY low = i_anla-anlkl
  BINARY SEARCH.

IF sy-subrc IS INITIAL.
  ASSIGN (vl_campo) TO <msfak>.
  IF <msfak> IS ASSIGNED.
    <msfak> = 1.
    UNASSIGN <msfak>.
  ENDIF.
  LOOP AT t_anlb INTO sl_anlb.

    vl_index = sy-tabix.
    MOVE 100 TO sl_anlb-aprop.
    MODIFY t_anlb FROM sl_anlb
      INDEX vl_index
      TRANSPORTING aprop.
    CLEAR sl_anlb.

  ENDLOOP.

ENDIF.
