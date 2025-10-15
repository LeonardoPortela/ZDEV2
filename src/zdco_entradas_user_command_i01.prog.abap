INCLUDE zdco_entradas_top.

*----------------------------------------------------------------------*
***INCLUDE ZDCO_ENTRADAS_USER_COMMAND_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT .

  CASE sy-ucomm.
    WHEN 'BACK' OR '%EX' OR 'RW'.
      PERFORM sair.
    WHEN 'PESQ'.
      PERFORM consulta_notas.
    WHEN 'VINC'.
      PERFORM vincula_notas.
    WHEN 'DESV'.
      PERFORM desvincula_notas.
    WHEN 'SAVE'.
      PERFORM salvar.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET TITLEBAR  '002'.
  SET PF-STATUS 'TELA'.

  vg_nr_dco          = wa_dco-nr_dco.
  vg_nome_fornecedor = wa_dco-nome_fornecedor.
  vg_nome_deposito   = wa_dco-nome_deposito.
  vg_nome_centro     = wa_dco-nome_centro.
  vg_nome_material   = wa_dco-nome_material.
  vg_ds_tipo_leilao  = wa_dco-ds_tipo_leilao.


  vg_qtd_disp = 0.
  LOOP AT it_disp INTO wa_disp.
    vg_qtd_disp = vg_qtd_disp + wa_disp-menge.
  ENDLOOP.

  vg_qtd_vinc = 0.
  LOOP AT it_vinc INTO wa_vinc.
    vg_qtd_vinc = vg_qtd_vinc + wa_vinc-menge.
  ENDLOOP.

  IF vg_dt_inicial IS INITIAL.
    vg_dt_inicial      = sy-datum.
  ENDIF.
  IF vg_dt_fim IS INITIAL.
    vg_dt_fim          = sy-datum.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consulta_notas .

  SELECT nf~docnum ni~itmnum nf~docdat nf~series nf~nfnum nf~branch ni~menge NF~NFENUM
    INTO CORRESPONDING FIELDS OF TABLE it_disp2
    FROM j_1bnfdoc AS nf
   INNER JOIN j_1bnflin AS ni ON ni~docnum EQ nf~docnum AND ni~matnr EQ wa_dco-cd_material
   WHERE nf~docdat GE vg_dt_inicial
     AND nf~docdat LE vg_dt_fim
     AND nf~parid  EQ wa_dco-id_fornecedor
     AND nf~branch EQ wa_dco-cd_centro
     AND nf~docnum NOT IN ( SELECT docnum FROM zdco_nf_entrada ).

  CLEAR it_disp.

  LOOP AT it_disp2 INTO wa_disp.

*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> INI
* CH  109593 - Ajuste_Z_1BNFE_MONITOR_F32_ZSDI0001
    IF WA_DISP-NFNUM eq '000000'.
        WA_disp-NFNUM     = WA_DISP-NFENUM.
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> END
    CONCATENATE wa_disp-docdat+6(2) '.' wa_disp-docdat+4(2) '.' wa_disp-docdat(4) INTO wa_disp-docdattxt.
    READ TABLE it_vinc INTO wa_vinc WITH KEY docnum = wa_disp-docnum itmnum = wa_disp-itmnum.
    IF sy-subrc NE 0.
      APPEND wa_disp TO it_disp.
    ENDIF.
  ENDLOOP.

  LOOP AT it_desvinculados INTO wa_disp.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> INI
* CH  109593 - Ajuste_Z_1BNFE_MONITOR_F32_ZSDI0001
    IF WA_DISP-NFNUM EQ '000000'.
        WA_disp-NFNUM     = WA_DISP-NFENUM.
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> END
    READ TABLE it_vinc INTO wa_vinc WITH KEY docnum = wa_disp-docnum itmnum = wa_disp-itmnum.
    IF sy-subrc NE 0.
      READ TABLE it_disp INTO wa_vinc WITH KEY docnum = wa_disp-docnum itmnum = wa_disp-itmnum.
      IF sy-subrc NE 0.
        APPEND wa_disp TO it_disp.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CONSULTA_NOTAS
*&---------------------------------------------------------------------*
*&      Form  VINCULA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vincula_notas .

  CLEAR: it_disp2, wa_vinc.

  DATA: vg_total LIKE zdco_nota-menge.

  PERFORM totaliza_vinculos USING vg_total.

  IF vg_total LE wa_dco-qt_material.
    LOOP AT it_disp INTO wa_disp.
      IF NOT wa_disp-mark IS INITIAL.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> INI
* CH  109593 - Ajuste_Z_1BNFE_MONITOR_F32_ZSDI0001
    IF WA_disp-NFNUM EQ '0'.
        wa_vinc-nfnum     = wa_disp-nfenum.
    else.
        wa_vinc-nfnum     = wa_disp-nfnum.
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavares - 12.09.2013 >>> END
        wa_vinc-docnum    = wa_disp-docnum.
        wa_vinc-itmnum    = wa_disp-itmnum.
        wa_vinc-docdat    = wa_disp-docdat.
        wa_vinc-docdattxt = wa_disp-docdattxt.
        wa_vinc-series    = wa_disp-series.
        wa_vinc-branch    = wa_disp-branch.
        wa_vinc-menge     = wa_disp-menge.
        APPEND wa_vinc TO it_vinc.
        vg_alterou = 'X'.
      ELSE.
        APPEND wa_disp TO it_disp2.
      ENDIF.
    ENDLOOP.
    it_disp[] = it_disp2[].
  ELSE.
    MESSAGE 'Não pode ser vinculada uma quantidade superior ao DCO!' TYPE 'I'.
  ENDIF.

ENDFORM.                    " VINCULA_NOTAS

*&---------------------------------------------------------------------*
*&      Form  DESVINCULA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desvincula_notas .

  LOOP AT it_vinc INTO wa_vinc WHERE mark EQ 'X'.
    IF vg_alterou IS INITIAL.
      CLEAR it_desvinculados.
    ENDIF.
    DELETE it_vinc INDEX sy-tabix.
    wa_vinc-mark = ' '.
    APPEND wa_vinc TO it_desvinculados.
    vg_alterou = 'X'.
  ENDLOOP.

  PERFORM consulta_notas.

ENDFORM.                    " DESVINCULA_NOTAS

*&---------------------------------------------------------------------*
*&      Form  SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair .
  IF NOT vg_alterou IS INITIAL.

    DATA: answer TYPE c LENGTH 1.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Foram alteradas as vinculações.'
        textline2 = 'Deseja salvar as alterações?'
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN 'J'.
        PERFORM salvar.
      WHEN 'N'.
        CLEAR vg_alterou.
      WHEN 'A'.
        EXIT.
    ENDCASE.

  ENDIF.

  IF vg_alterou IS INITIAL.

    CALL FUNCTION 'ZDEQUEUE_ZDCO_NOTA_ENTRADA'
      EXPORTING
        mode_zdco_vinc = 'X'
        mandt          = sy-mandt
        nu_dco         = wa_dco-nu_dco
      EXCEPTIONS
        OTHERS         = 1.

    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDFORM.                    " SAIR

*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar .

  DATA v_docnum TYPE j_1bdocnum.

  DATA: BEGIN OF wa_nf_entrada.
          INCLUDE STRUCTURE zdco_nf_entrada.
  DATA: END OF wa_nf_entrada.

  DATA: it_nf_entrada LIKE STANDARD TABLE OF wa_nf_entrada.

  CLEAR wa_vinc.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_nf_entrada
    FROM zdco_nf_entrada
   WHERE nu_dco EQ wa_dco-nu_dco.

  LOOP AT it_nf_entrada INTO wa_nf_entrada.

    READ TABLE it_vinc INTO wa_vinc WITH KEY docnum = wa_nf_entrada-docnum itmnum = wa_nf_entrada-itmnum.
    IF sy-subrc NE 0.
      DELETE FROM zdco_nf_entrada WHERE nu_dco EQ wa_dco-nu_dco AND docnum EQ wa_nf_entrada-docnum AND itmnum = wa_nf_entrada-itmnum.
    ENDIF.

  ENDLOOP.

  LOOP AT it_vinc INTO wa_vinc.

    SELECT SINGLE docnum
      INTO v_docnum
      FROM zdco_nf_entrada
     WHERE nu_dco EQ wa_dco-nu_dco
       AND docnum EQ wa_vinc-docnum
       AND itmnum EQ wa_vinc-itmnum.

    IF sy-subrc NE 0.
      CLEAR wa_nf_entrada.
      wa_nf_entrada-nu_dco  = wa_dco-nu_dco.
      wa_nf_entrada-docnum  = wa_vinc-docnum.
      wa_nf_entrada-itmnum  = wa_vinc-itmnum.
      wa_nf_entrada-qt_nota = wa_vinc-menge.
      INSERT into zdco_nf_entrada values wa_nf_entrada.
    ENDIF.

  ENDLOOP.

  DATA: vg_total LIKE zdco_nota-menge.

  PERFORM totaliza_vinculos USING vg_total.

  UPDATE zdco_produtor SET qt_entregue = vg_total
   WHERE nu_dco EQ wa_dco-nu_dco.

  COMMIT WORK.

  CLEAR: vg_alterou, it_desvinculados.

ENDFORM.                    " SALVAR

*&---------------------------------------------------------------------*
*&      Form  TOTALIZA_VINCULOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_TOTAL  text
*----------------------------------------------------------------------*
FORM totaliza_vinculos  USING    p_vg_total.

  DATA: vg_total LIKE zdco_nota-menge,
        vg_vincu LIKE zdco_nota-menge.

  CLEAR: vg_total, vg_vincu.

  vg_total = 0.
  vg_vincu = 0.
  LOOP AT it_vinc INTO wa_vinc.
    vg_total = vg_total + wa_vinc-menge.
  ENDLOOP.

  LOOP AT it_disp INTO wa_disp.
    IF NOT wa_disp-mark IS INITIAL.
      vg_vincu = vg_vincu + wa_disp-menge.
    ENDIF.
  ENDLOOP.

  p_vg_total = vg_total + vg_vincu.


ENDFORM.                    " TOTALIZA_VINCULOS
