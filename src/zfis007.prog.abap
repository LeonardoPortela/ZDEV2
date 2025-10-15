*&---------------------------------------------------------------------*
*& Report  ZFIS007
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfis007.

PARAMETERS: pm_bukrs   LIKE t001-bukrs OBLIGATORY, "  DEFAULT '0001',
            pm_gjahr   LIKE bsis-gjahr OBLIGATORY. " DEFAULT '2008'.
RANGES: rg_taxtyp      FOR  j_1bnfstx-taxtyp.

DATA: wg_dat_ini   LIKE sy-datum,
      wg_dat_fim   LIKE sy-datum,
      wg_fm_name   TYPE rs38l_fnam,
      wg_cfop(10).

DATA: wg_branch    LIKE  j_1bbranch-branch,
      wa_sadr      LIKE sadr,
      wa_branch    LIKE j_1bbranch,
      wg_acum      LIKE zst_zfis007_det-acum,
      wg_cgc       LIKE j_1bwfield-cgc_number.

DATA: it_t247      LIKE t247 OCCURS 0 WITH HEADER LINE.
DATA: it_out       LIKE zst_zfis007_res OCCURS 0 WITH HEADER LINE,
      it_tax       LIKE zst_zfis007_det OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_bsis OCCURS 0,
       budat       LIKE bsis-budat,
       dmbtr       LIKE bsis-dmbtr,
      END OF it_bsis.

START-OF-SELECTION.


  PERFORM f_general_data.
  PERFORM f_get_data.
  PERFORM f_process_data.
  PERFORM f_output.
*&---------------------------------------------------------------------*
*&      Form  f_general_data
*&---------------------------------------------------------------------*
FORM f_general_data .

  CALL FUNCTION 'J_1B_COMPANY_READ'
    EXPORTING
      company = pm_bukrs
    IMPORTING
      branch  = wg_branch
    EXCEPTIONS
      OTHERS  = 3.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e782(8z) WITH wg_branch pm_bukrs.
  ENDIF.
  CLEAR: wa_sadr, wg_cgc, wa_branch.
  CALL FUNCTION 'J_1B_BRANCH_READ'
    EXPORTING
      branch        = wg_branch
      company       = pm_bukrs
    IMPORTING
      address       = wa_sadr
      branch_record = wa_branch
      cgc_number    = wg_cgc
    EXCEPTIONS
      OTHERS        = 4.
*  check sy-subrc is initial.
  IF NOT sy-subrc IS INITIAL.
    MESSAGE e782(8z) WITH wg_branch pm_bukrs.
  ENDIF.

  wg_dat_ini = pm_gjahr.
  wg_dat_ini+4 = '0101'.

  wg_dat_fim = pm_gjahr.
  wg_dat_fim+4 = '1231'.

  APPEND 'IEQICM0'  TO rg_taxtyp.
  APPEND 'IEQICM1'  TO rg_taxtyp.
  APPEND 'IEQICM2'  TO rg_taxtyp.
  APPEND 'IEQICM3'  TO rg_taxtyp.
  APPEND 'IEQICMF'  TO rg_taxtyp.
  APPEND 'IEQIFR1'  TO rg_taxtyp.
  APPEND 'IEQIC1O'  TO rg_taxtyp.
  APPEND 'IEQICM4'  TO rg_taxtyp.
  APPEND 'IEQICMN'  TO rg_taxtyp.
  APPEND 'IEQICMO'  TO rg_taxtyp.
  APPEND 'IEQICMX'  TO rg_taxtyp.

  CALL FUNCTION 'MONTH_NAMES_GET'
    TABLES
      month_names = it_t247
    EXCEPTIONS
      OTHERS      = 2.
*  LOOP AT it_t247.
*    CLEAR it_out.
*    it_out-mes = it_t247-mnr.
*    it_out-nommes = it_t247-ltx.
*    APPEND it_out.
*  ENDLOOP.
ENDFORM.                    " f_general_data
*&---------------------------------------------------------------------*
*&      Form  f_output
*&---------------------------------------------------------------------*
FORM f_output.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = 'ZFIS007_FORM'
    IMPORTING
      fm_name  = wg_fm_name
    EXCEPTIONS
      OTHERS   = 3.
  CHECK sy-subrc IS INITIAL.
  CALL FUNCTION wg_fm_name
    EXPORTING
      sadr   = wa_sadr
      cgc    = wg_cgc
      branch = wa_branch
      gjahr  = pm_gjahr
    TABLES
      resumo = it_out
      notas  = it_tax
    EXCEPTIONS
      OTHERS = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "f_output
*&---------------------------------------------------------------------*
*&      Form  f_output2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_output2.
  LOOP AT it_out.
    WRITE:/
            it_out-mes,       sy-vline,
            it_out-nommes,    sy-vline,
            it_out-trib,      sy-vline,
            it_out-tot,       sy-vline,
            it_out-coef,      sy-vline,
            it_out-sald,      sy-vline,
            it_out-frac,      sy-vline,
            it_out-cred,      sy-vline.
  ENDLOOP.
  ULINE.
  SKIP 4.
  ULINE.
  LOOP AT it_tax.
    WRITE:/ sy-vline,
            it_tax-pstdat,  sy-vline,
            it_tax-nfnum,   sy-vline,
            it_tax-maktx,   sy-vline,
            it_tax-base,    sy-vline,
            '            ', sy-vline,
            '            ', sy-vline.
  ENDLOOP.
  ULINE.
ENDFORM.                    " f_output
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
FORM f_get_data .

  SELECT j_1bnfstx~docnum j_1bnfstx~itmnum j_1bnfstx~taxtyp
         j_1bnfstx~base   j_1bnfstx~excbas j_1bnfstx~othbas
         j_1bnfstx~taxval
         j_1bnfdoc~pstdat j_1bnfdoc~nfnum  j_1bnfdoc~waerk
         j_1bnflin~cfop   j_1bnflin~maktx
  FROM j_1bnfdoc
           INNER JOIN j_1bnflin ON j_1bnfdoc~docnum EQ j_1bnflin~docnum
           INNER JOIN j_1bnfstx ON j_1bnflin~docnum EQ j_1bnfstx~docnum
                               AND j_1bnflin~itmnum EQ j_1bnfstx~itmnum
  INTO TABLE it_tax
  WHERE j_1bnfdoc~pstdat BETWEEN wg_dat_ini AND wg_dat_fim
    AND j_1bnfstx~taxtyp IN      rg_taxtyp.

  SELECT budat dmbtr
    FROM bsis
    INTO TABLE it_bsis
   WHERE budat BETWEEN wg_dat_ini AND wg_dat_fim
     AND hkont EQ '0000113212'.
ENDFORM.                    " f_get_data
*&---------------------------------------------------------------------*
*&      Form  f_process_data
*&---------------------------------------------------------------------*
FORM f_process_data .
  LOOP AT it_bsis.
    CLEAR it_out.
    it_out-mes = it_bsis-budat+4(2).
    it_out-sald = it_bsis-dmbtr.
    COLLECT it_out.
  ENDLOOP.

  LOOP AT it_tax.
    CLEAR it_out.
    it_out-mes = it_tax-pstdat+4(2).
    it_out-trib = it_tax-base + it_tax-excbas + it_tax-othbas.
    WRITE it_tax-cfop TO wg_cfop.
    IF wg_cfop(1) = '7'.
      IF NOT it_tax-taxval IS INITIAL.
        it_out-tot  = it_tax-base.
      ENDIF.
    ENDIF.
    COLLECT it_out.
  ENDLOOP.

  LOOP AT it_out.
* nome do mes
    READ TABLE it_t247 WITH KEY mnr = it_out-mes.
    CHECK sy-subrc IS INITIAL.
    it_out-nommes = it_t247-ltx.
* Coeficiente
    IF NOT it_out-trib  IS INITIAL.
      it_out-coef = it_out-tot / it_out-trib.
    ELSE.
      CLEAR it_out-coef.
    ENDIF.
*** Saldo de Crédito
**    it_out-sald = 0.
* Fração Mensal (Fixo)
    it_out-frac = '0.02083'.
* Crédito a ser apropriado
    it_out-cred = it_out-sald * it_out-coef.
    MODIFY it_out.
  ENDLOOP.
  LOOP AT it_t247.
    READ TABLE it_out WITH KEY mes = it_t247-mnr.
    CHECK NOT sy-subrc IS INITIAL.
    CLEAR it_out.
    it_out-mes = it_t247-mnr.
    it_out-nommes = it_t247-ltx.
    APPEND it_out.
  ENDLOOP.

  SORT it_out BY mes.
  SORT it_tax BY pstdat nfnum.
  CLEAR wg_acum.
  LOOP AT it_tax.
    ADD it_tax-base TO wg_acum.
    it_tax-acum = wg_acum.
    MODIFY it_tax.
  ENDLOOP.
ENDFORM.                    " f_process_data
