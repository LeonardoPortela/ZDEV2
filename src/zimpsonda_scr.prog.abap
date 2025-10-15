*&---------------------------------------------------------------------*
*& Include          ZIMPSONDA_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs     FOR  t001-bukrs.
  SELECT-OPTIONS: p_lote      FOR  wa_zimp_cad_lote-lote   ,
                  p_dtv       FOR  wa_zimp_lanc_impost-dt_venc,
                  p_dtl       FOR  wa_zimp_lanc_impost-dt_venc,
                  p_cod       FOR  wa_zimp_lanc_impost-cod_imposto,                   "CS2017000143
                  p_user      FOR  wa_zimp_cad_lote-usnam,                            "CS2017000143
                  p_dep       FOR  wa_zimp_cad_lote-dep_resp DEFAULT '84'.                         "CS2017000143
  PARAMETERS:
    p_lot1 RADIOBUTTON GROUP rad1,
    p_lot2 RADIOBUTTON GROUP rad1,
    p_lot3 RADIOBUTTON GROUP rad1,
    p_lot4 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_button USER-COMMAND click.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_butt USER-COMMAND click1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_tp USER-COMMAND click5.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_lanc USER-COMMAND click3.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_guia USER-COMMAND click4.
SELECTION-SCREEN: END OF BLOCK b3.


SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN PUSHBUTTON /2(25)
    p_del USER-COMMAND click2.
SELECTION-SCREEN: END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cod-low.
  PERFORM f4_p_cod.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cod-high.
  PERFORM f4_p_cod.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_user-low.
  PERFORM f4_p_user.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_user-high.
  PERFORM f4_p_user.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dep-low.
  PERFORM f4_p_dep.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dep-high.
  PERFORM f4_p_dep.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'CLICK'.
    CALL TRANSACTION 'ZIMPCODIMP'.
  ELSEIF sy-ucomm = 'CLICK1'.
    CALL TRANSACTION 'ZIMPCODOBRIG'.
  elseif sy-ucomm = 'CLICK5'.
    CALL TRANSACTION 'ZIMPTPAJUSTE'.
  ELSEIF sy-ucomm = 'CLICK2'.
    DELETE FROM zimp_lanc_sonda WHERE NOT iaj_cod_matriz IS NULL.
    DELETE FROM zimp_guia_sonda WHERE NOT igu_cod_matriz IS NULL.
    MESSAGE 'Dados excluidos com sucesso' TYPE 'S'.
  ELSEIF sy-ucomm = 'CLICK3'.
    SELECT * FROM zimp_lanc_sonda INTO TABLE it_zimp_lanc_sonda.
    IF it_zimp_lanc_sonda IS INITIAL.
     MESSAGE 'Não existem dados na tabela' TYPE 'I'.
    ELSE.
      cl_salv_table=>factory(
       IMPORTING
         r_salv_table = go_alv
       CHANGING
         t_table      = it_zimp_lanc_sonda
       ).
      go_alv->display( ).
    ENDIF.
  ELSEIF sy-ucomm = 'CLICK4'.
    SELECT * FROM zimp_guia_sonda INTO TABLE it_zimp_guia_sonda.
    IF it_zimp_lanc_sonda IS INITIAL.
     MESSAGE 'Não existem dados na tabela' TYPE 'I'.
    ELSE.
      cl_salv_table=>factory(
       IMPORTING
         r_salv_table = go_alv
       CHANGING
         t_table      = it_zimp_guia_sonda
       ).
      go_alv->display( ).
    ENDIF.
  ENDIF.
