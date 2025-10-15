*&---------------------------------------------------------------------*
*& Include          ZFIS46_TOP
*&---------------------------------------------------------------------*
TABLES: j_1bnfdoc,lfa1,j_1bnflin,zib_nfe_forn,sscrfields, t001w.

DATA: icon_proc TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
  SELECT-OPTIONS: p_bukrs   FOR j_1bnfdoc-bukrs  NO-EXTENSION NO INTERVALS. "OBLIGATORY
  SELECT-OPTIONS: p_werks   FOR j_1bnfdoc-branch NO-EXTENSION NO INTERVALS.
  SELECT-OPTIONS: p_regio   FOR t001w-regio NO INTERVALS. "AJUSTE - ACRESCENTADO FILTRO UF - MMSILVA - 11.03.2025
  SELECT-OPTIONS: p_pstdat  FOR j_1bnfdoc-pstdat NO-EXTENSION .
  SELECT-OPTIONS: p_docdat  FOR j_1bnfdoc-docdat NO-EXTENSION.
  SELECT-OPTIONS: p_docnum  FOR j_1bnfdoc-docnum .
  SELECT-OPTIONS: p_lifnr   FOR lfa1-lifnr .
  SELECT-OPTIONS: p_cfop    FOR j_1bnflin-cfop .
  SELECT-OPTIONS: p_chave   FOR zib_nfe_forn-nu_chave.
SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-002 .
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1,
*              r2 RADIOBUTTON GROUP rad1, "DEFAULT 'X',
*              r3 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN END OF BLOCK part2.

*SELECTION-SCREEN BEGIN OF BLOCK part3 WITH FRAME TITLE TEXT-003 .
*  PARAMETERS: p_vriant LIKE tline-tdline VISIBLE LENGTH 25.
*SELECTION-SCREEN END OF BLOCK part3.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.

* BUG # - MMSILVA - 19.05.2025 - Inicio
  DATA: _param TYPE  ustyp_t_parameters.

  "Pega os Parametros do Usuario
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  READ TABLE _param INTO DATA(w_param) WITH KEY parid = 'ZFIS70_PARAMS'.
  IF sy-subrc IS INITIAL.
    icon_proc = icon_parameter_changing && 'Parâmetros'.
    sscrfields-functxt_01 = icon_proc .
  ENDIF.
* BUG # - MMSILVA - 19.05.2025 - Fim

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      SUBMIT ZFIS46_params AND RETURN.
    WHEN 'ONLI'.
      IF p_bukrs IS INITIAL.
        MESSAGE 'O campo Empresa é Obrigatório!' TYPE 'E'.
        EXIT.
      ENDIF.

       "AJUSTE - RETIRAR OBRIGATORIEDADE DO PARÂMETRO DATA DO DOCUMENTO - MMSILVA - 28.01.2025 - Inicio
*      IF p_docdat[] IS NOT INITIAL.
*
*        IF p_docdat-low = '00000000' AND p_docdat-high = '00000000'.
*          MESSAGE 'O campo Data do documento inicio e fim é Obrigatório!' TYPE 'E'.
*          EXIT.
*        ELSE.
*          IF p_docdat-low = '00000000'.
*            MESSAGE 'O campo Data do documento Inicio Obrigatório!' TYPE 'E'.
*            EXIT.
*          ELSEIF p_docdat-high = '00000000'.
*            MESSAGE 'O campo Data do documento fim é Obrigatório!' TYPE 'E'.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        MESSAGE 'O campo Data do documento é Obrigatório!' TYPE 'E'.
*        EXIT.
*      ENDIF.
      "AJUSTE - RETIRAR OBRIGATORIEDADE DO PARÂMETRO DATA DO DOCUMENTO - MMSILVA - 28.01.2025 - Fim

      IF p_pstdat[] IS NOT INITIAL.

        IF p_pstdat-low = '00000000' AND  p_pstdat-high = '00000000'.
          MESSAGE 'O campo Data de Lançamento inicio e fim é Obrigatório!' TYPE 'E'.
          EXIT.
        ELSE.
          IF p_pstdat-low = '00000000'.
            MESSAGE 'O campo Data de Lançamento Inicio Obrigatório!' TYPE 'E'.
            EXIT.
          ELSEIF p_pstdat-high = '00000000'.
            MESSAGE 'O campo Data de Lançamento fim é Obrigatório!' TYPE 'E'.
            EXIT.
          ENDIF.
        ENDIF.

      ELSE.
        MESSAGE 'O campo Data de Lançamento é Obrigatório!' TYPE 'E'.
        EXIT.
      ENDIF.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  CALL SELECTION-SCREEN '0100'.
