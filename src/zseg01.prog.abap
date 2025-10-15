*&---------------------------------------------------------------------*
*& Report  ZSE01_APROPR_SEGUROS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zse01_apropr_seguros.

TABLES zse01_apropr_seg.

DATA: BEGIN OF t_se01 OCCURS 0.
        INCLUDE STRUCTURE zse01_apropr_seg.
DATA: END OF t_se01.

TYPES: BEGIN OF ty_saida,
  chave TYPE zse01_apropr_seg-chave,
  bukrs TYPE zse01_apropr_seg-bukrs,
  dbbdt TYPE zse01_apropr_seg-dbbdt,
  dbedt TYPE zse01_apropr_seg-dbedt,
  dbmon TYPE zse01_apropr_seg-dbmon,
  dbtag TYPE zse01_apropr_seg-dbtag,
  xfwhw TYPE zse01_apropr_seg-xfwhw,
  xbllt TYPE zse01_apropr_seg-xbllt,
  blart TYPE zse01_apropr_seg-blart,
  xblnr TYPE zse01_apropr_seg-xblnr,
  bktxt TYPE zse01_apropr_seg-bktxt,
  waers TYPE zse01_apropr_seg-waers,
  wwert TYPE zse01_apropr_seg-wwert,
  newbs_d TYPE zse01_apropr_seg-newbs_d,
  newko_d TYPE zse01_apropr_seg-newko_d,
  wrbtr_d TYPE zse01_apropr_seg-wrbtr_d,
  bupla_d TYPE zse01_apropr_seg-bupla_d,
  kostl_d TYPE zse01_apropr_seg-kostl_d,
  aufnr_d TYPE zse01_apropr_seg-aufnr_d,
  gsber_d TYPE zse01_apropr_seg-gsber_d,
  zuonr_d TYPE zse01_apropr_seg-zuonr_d,
  sgtxt_d TYPE zse01_apropr_seg-sgtxt_d,
  newbs_c TYPE zse01_apropr_seg-newbs_c,
  newko_c TYPE zse01_apropr_seg-newko_c,
  wrbtr_c TYPE zse01_apropr_seg-wrbtr_d,
  bupla_c TYPE zse01_apropr_seg-bupla_d,
  gsber_c TYPE zse01_apropr_seg-gsber_d,
  zuonr_c TYPE zse01_apropr_seg-zuonr_d,
  sgtxt_c TYPE zse01_apropr_seg-sgtxt_d,
  cpudt   TYPE zse01_apropr_seg-cpudt,    "ele ta falando q esses campos nao exitem
  cputm   TYPE zse01_apropr_seg-cputm,
  usnam   TYPE zse01_apropr_seg-usnam,
  END OF ty_saida,

    BEGIN OF ty_saida1,
  chave TYPE zse01_apropr_seg-chave,
  zuonr_c TYPE zse01_apropr_seg-zuonr_d,
      END OF ty_saida1.

DATA: t_saida TYPE TABLE OF ty_saida,
      t_saida1 TYPE TABLE OF ty_saida1.

DATA: wa_saida TYPE ty_saida,
      wa_saida1 TYPE ty_saida1.


DATA: wa_planilha LIKE  alsmex_tabline,
      it_planilha  LIKE STANDARD TABLE OF wa_planilha.


SELECTION-SCREEN BEGIN OF BLOCK  b1 WITH FRAME TITLE text-h01.
PARAMETERS: p_lgpbe  LIKE rlgrap-filename  OBLIGATORY.
SELECTION-SCREEN END  OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lgpbe.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\'
      mask             = '*.*'
      mode             = '0'
      title            = 'Busca de Arquivo'
    IMPORTING
      filename         = p_lgpbe
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

START-OF-SELECTION.

  PERFORM f_ler_arquivo.
  PERFORM seleciona_dados.
*&---------------------------------------------------------------------*
*&      Form  F_LER_ARQUIVO
*&---------------------------------------------------------------------*
FORM f_ler_arquivo .

  DATA: w_uzeit TYPE sy-uzeit.

  CLEAR: t_se01.
  REFRESH: t_se01, t_saida, it_planilha.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_lgpbe
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 999
      i_end_row               = 999
    TABLES
      intern                  = it_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT it_planilha INTO wa_planilha.
    CASE wa_planilha-col.
      WHEN 1.
        MOVE wa_planilha-value TO wa_saida-chave.

      WHEN 2.
        MOVE wa_planilha-value TO wa_saida-bukrs.

      WHEN 3.
        CONCATENATE wa_planilha-value+6(4)
                            wa_planilha-value+3(2)
                            wa_planilha-value(2)
                            INTO wa_saida-dbbdt.
*        MOVE wa_planilha-value TO wa_saida-dbbdt.

      WHEN 4.

        CONCATENATE wa_planilha-value+6(4)
                    wa_planilha-value+3(2)
                    wa_planilha-value(2)
                    INTO wa_saida-dbedt.
*        MOVE wa_planilha-value TO wa_saida-dbedt.

      WHEN 5.
        MOVE wa_planilha-value TO wa_saida-dbmon.

      WHEN 6.
        MOVE wa_planilha-value TO wa_saida-dbtag.

      WHEN 7.
        MOVE wa_planilha-value TO wa_saida-xfwhw.

      WHEN 8.
        MOVE wa_planilha-value TO wa_saida-xbllt.

      WHEN 9.
        MOVE wa_planilha-value TO wa_saida-blart.

      WHEN 10.
        MOVE wa_planilha-value TO wa_saida-xblnr.

      WHEN 11.
        MOVE wa_planilha-value TO wa_saida-bktxt.

      WHEN 12.
        MOVE wa_planilha-value TO wa_saida-waers.

      WHEN 13.
        CONCATENATE wa_planilha-value+6(4)
                            wa_planilha-value+3(2)
                            wa_planilha-value(2)
                            INTO wa_saida-wwert.
*        MOVE wa_planilha-value TO wa_saida-wwert.

      WHEN 14.
        MOVE wa_planilha-value TO wa_saida-newbs_d.

      WHEN 15.
**
        MOVE wa_planilha-value TO wa_saida-newko_d.

      WHEN 16.
        TRANSLATE wa_planilha-value USING '. '.
        CONDENSE wa_planilha-value NO-GAPS.
        TRANSLATE wa_planilha-value USING ',.'.
        MOVE wa_planilha-value TO wa_saida-wrbtr_d.

      WHEN 17.
        MOVE wa_planilha-value TO wa_saida-bupla_d.

      WHEN 18.
        MOVE wa_planilha-value TO wa_saida-kostl_d.

      WHEN 19.
        MOVE wa_planilha-value TO wa_saida-aufnr_d.

      WHEN 20.
        MOVE wa_planilha-value TO wa_saida-gsber_d.

      WHEN 21.
        MOVE wa_planilha-value TO wa_saida-zuonr_d.

      WHEN 22.
        MOVE wa_planilha-value TO wa_saida-sgtxt_d.

      WHEN 23.
        MOVE wa_planilha-value TO wa_saida-newbs_c.

      WHEN 24.
        MOVE wa_planilha-value TO wa_saida-newko_c.

      WHEN 25.
        TRANSLATE wa_planilha-value USING '. '.
        CONDENSE wa_planilha-value NO-GAPS.
        TRANSLATE wa_planilha-value USING ',.'.
        MOVE wa_planilha-value TO wa_saida-wrbtr_c.

      WHEN 26.
        MOVE wa_planilha-value TO wa_saida-bupla_c.

      WHEN 27.
        MOVE wa_planilha-value TO wa_saida-gsber_c.

      WHEN 28.
        MOVE wa_planilha-value TO wa_saida-zuonr_c.

      WHEN 29.
        MOVE wa_planilha-value TO wa_saida-sgtxt_c.

*      WHEN 30.
*        MOVE wa_planilha-value TO wa_saida-cpudt.
*
*      WHEN 31.
*        MOVE wa_planilha-value TO wa_saida-cputm.
*
*      WHEN 32.
*        MOVE wa_planilha-value TO wa_saida-usnam.
        APPEND wa_saida TO t_saida.

    ENDCASE.
  ENDLOOP.

  w_uzeit = sy-uzeit.

  LOOP AT t_saida INTO wa_saida.
    wa_saida-cpudt = sy-datum.
    wa_saida-cputm = w_uzeit.
    wa_saida-usnam = sy-uname.
    MODIFY t_saida FROM wa_saida INDEX sy-tabix.
    MOVE-CORRESPONDING wa_saida TO t_se01.
    APPEND t_se01.
  ENDLOOP.


ENDFORM.                    " F_LER_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  SELECT chave zuonr_d
    FROM zse01_apropr_seg
    INTO TABLE t_saida1
    FOR ALL ENTRIES IN t_saida
    WHERE chave EQ t_saida-chave
    AND zuonr_d EQ t_saida-zuonr_d.
  IF sy-subrc IS INITIAL.
    MESSAGE i398(00) WITH 'Existe registro na tabela, processo cancelado!'.
    STOP.
  ELSE.
    MODIFY zse01_apropr_seg FROM TABLE t_se01.
    COMMIT WORK AND WAIT.
    MESSAGE i398(00) WITH 'Tabela carregada!'.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
