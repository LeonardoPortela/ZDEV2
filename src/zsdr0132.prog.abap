*&---------------------------------------------------------------------*
*& Report  ZSDR0132                                                    *&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Realização de Baixa de Volume de NF de compra           *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0132.

*******************************************************************************************
* Declaração de Constantes
*******************************************************************************************
CONSTANTS: BEGIN OF c_const,
             tabela_cad TYPE tabname VALUE 'ZSDT0278',
             layout_cad TYPE tabname VALUE 'ZSDT0278_OUT',
             tela_cad   TYPE c LENGTH 4 VALUE '0098',
             titulo_cad TYPE cua_tit_tx VALUE 'Cadastro Aprovador -  Baixa de Notas Fiscais de Compra',
             set_user   TYPE setleaf-setname VALUE 'ZSDT0182_USER',
           END OF c_const.

*******************************************************************************************
* Tabelas
*******************************************************************************************
TABLES: j_1bnfdoc,
        j_1bnflin,
        zsdt0276.

*******************************************************************************************
* Tela selecao
*******************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS    : p_lancar RADIOBUTTON                     GROUP g1 USER-COMMAND usr1 DEFAULT 'X',
                p_listar RADIOBUTTON                     GROUP g1,
                p_aprova RADIOBUTTON                     GROUP g1,
                p_cadast RADIOBUTTON                     GROUP g1.
SELECTION-SCREEN: END   OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs      FOR j_1bnfdoc-bukrs         MODIF ID t1 NO INTERVALS NO-EXTENSION,
                s_branch     FOR j_1bnfdoc-branch        MODIF ID t1,
                s_docnum     FOR j_1bnfdoc-docnum        MODIF ID t4 NO INTERVALS,
                s_docdat     FOR j_1bnfdoc-docdat        MODIF ID t4,
                s_dtbaix     FOR zsdt0276-dt_baixa       MODIF ID t3,
                s_lifnr      FOR j_1bnfdoc-parid         MODIF ID t4 NO INTERVALS,
                s_matnr      FOR j_1bnflin-matnr         MODIF ID t4 NO INTERVALS,
                s_status     FOR zsdt0276-status         MODIF ID t2 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END   OF BLOCK b2.

*******************************************************************************************
* Tratar tela
*******************************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN p_lancar.
        IF screen-group1   = 'T2' OR
           screen-group1   = 'T3'.
          screen-active    = 0.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN p_aprova.
        IF screen-group1   = 'T2' OR
           screen-group1   = 'T4'.
          screen-active    = 0.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      WHEN p_cadast.
        IF screen-group1   = 'T1' OR
           screen-group1   = 'T2' OR
           screen-group1   = 'T3' OR
           screen-group1   = 'T4'.
          screen-active    = 0.
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

*******************************************************************************************
* tratar empresa
*******************************************************************************************
AT SELECTION-SCREEN.

  CHECK sy-ucomm <> 'USR1'.

  IF p_cadast IS INITIAL.

    IF s_bukrs[] IS INITIAL.
      MESSAGE s000(z01) WITH text-003 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF p_lancar = abap_true.
      IF s_docnum[] IS INITIAL AND s_docdat[] IS INITIAL.
        MESSAGE s000(z01) WITH text-004 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.

    IF p_listar = abap_true.
      IF s_docnum[] IS INITIAL AND
        ( s_docdat[] IS INITIAL AND
          s_dtbaix[] IS INITIAL ).
        MESSAGE s000(z01) WITH text-005 text-007 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.

    IF p_aprova = abap_true.
      IF s_dtbaix[] IS INITIAL.
        MESSAGE s000(z01) WITH text-006 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
* inicio
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  CASE abap_true.

    WHEN p_lancar.

*--------------------------------------------
* lancamento da baixa
*--------------------------------------------
      SUBMIT zsdr0133
        WITH s_bukrs   IN s_bukrs
        WITH s_branch  IN s_branch
        WITH s_docnum  IN s_docnum
        WITH s_docdat  IN s_docdat
        WITH s_lifnr   IN s_lifnr
        WITH s_matnr   IN s_matnr
         AND RETURN.

    WHEN p_listar.
      PERFORM zf_listar_baixa.

    WHEN p_aprova.
      PERFORM zf_aprovar_baixa.

    WHEN p_cadast.
      PERFORM zf_cadastro_aprovador.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  ZF_CADASTRO_APROVADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_cadastro_aprovador .

  DATA: l_msg TYPE bapiret2-message.

* Verifica permissão do usuário para efetuar cadastro
  SELECT SINGLE *
    FROM setleaf INTO @DATA(w_setleaf)
   WHERE setname = @c_const-set_user
     AND valfrom = @sy-uname.

  IF sy-subrc IS INITIAL.
    SUBMIT zregister_data WITH p_db_tab = c_const-tabela_cad
                          WITH p_stcnam = c_const-layout_cad
                          WITH p_scmant = c_const-tela_cad
                          WITH p_title  = c_const-titulo_cad
                      AND RETURN.

  ELSE.
    CONCATENATE sy-uname 'você não tem acesso para essa opção, procure a Área Execução Corporativa'
    INTO l_msg SEPARATED BY space.
    MESSAGE  l_msg TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_APROVAR_BAIXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_aprovar_baixa .

  IF s_dtbaix[] IS NOT INITIAL.
    SUBMIT zsdr0132_aprovar
      WITH s_bukrs   IN s_bukrs
      WITH s_branch  IN s_branch
      WITH s_dtbaix  IN s_dtbaix AND RETURN.
  ELSE.
    MESSAGE 'Informar data de baixa!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LISTAR_BAIXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_listar_baixa .

  IF s_bukrs[] IS INITIAL.
    MESSAGE s000(z01) WITH text-003 DISPLAY LIKE 'E'.
  ENDIF.

  IF s_docnum[] IS INITIAL AND
   ( s_docdat[] IS INITIAL AND
     s_dtbaix[] IS INITIAL ).
    MESSAGE s000(z01) WITH text-005 text-007 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.


  IF s_bukrs[] IS NOT INITIAL
    AND ( s_docnum[] IS NOT INITIAL OR s_dtbaix[] IS NOT INITIAL OR s_docdat[] IS NOT INITIAL ).

    SUBMIT zsdr0132_listar
      WITH s_bukrs   IN s_bukrs
      WITH s_branch  IN s_branch
      WITH s_docnum  IN s_docnum
      WITH s_docdat  IN s_docdat
      WITH s_dtbaix  IN s_dtbaix
      WITH s_lifnr   IN s_lifnr
      WITH s_matnr   IN s_matnr
      WITH s_status  IN s_status
       AND RETURN.

  ENDIF.


ENDFORM.
