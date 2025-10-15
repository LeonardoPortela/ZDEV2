FUNCTION zhcmf_return_gestor_2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PERNR_SOLICITANTE) TYPE  PERSNO
*"     REFERENCE(TIPO) TYPE  CHAR1
*"     REFERENCE(CPF_GESTOR) TYPE  PBR_CPFNR OPTIONAL
*"     REFERENCE(NOME_GESTOR) TYPE  PAD_CNAME OPTIONAL
*"     REFERENCE(PERNR_GESTOR) TYPE  PERSNO OPTIONAL
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMT_E_UNIORG
*"----------------------------------------------------------------------
  DATA: t_zhcmt_f_uniorg TYPE TABLE OF zhcmt_f_uniorg.
  DATA: cpf14      TYPE char14.
  DATA: cpf11      TYPE char11.
  DATA: vl_nome    TYPE pad_cname.
  RANGES:  r_cpf_gestor   FOR pa0465-cpf_nr,
           r_nome_gestor  FOR zhcmt_pa_0031-nome,
           r_pernr_gestor FOR pa0001-pernr,
           r_orgeh        FOR zhcmt_pa_0031-cod_uniorg,
           r_abkrs        FOR pa0001-abkrs.

  CHECK pernr_solicitante IS NOT INITIAL.
  CHECK tipo IS NOT INITIAL.

  IF cpf_gestor IS NOT INITIAL.
    FREE r_cpf_gestor.

    cpf14 = cpf_gestor.
    REPLACE ALL OCCURRENCES OF '.' IN cpf14 WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN cpf14 WITH ''.
    cpf11 = cpf14.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = cpf11    " CPF in internal format (NUMC 11)
      IMPORTING
        output = cpf14.  " CPF in screen format (999.999.999-99)

    APPEND VALUE #(
                    sign = 'I'
                    option = 'EQ'
                    low = cpf14
                  ) TO r_cpf_gestor.
  ENDIF.

  IF nome_gestor IS NOT INITIAL.
    FREE r_nome_gestor.
    vl_nome = nome_gestor.

    REPLACE ' ' WITH '*' INTO vl_nome.

    APPEND VALUE #(
                    sign = 'I'
                    option = 'CP'
                    low = |*{ vl_nome CASE = UPPER }*|
                  ) TO r_nome_gestor.
  ENDIF.

  IF pernr_gestor IS NOT INITIAL.
    FREE r_pernr_gestor.
    APPEND VALUE #(
                    sign = 'I'
                    option = 'EQ'
                    low = pernr_gestor
                  ) TO r_pernr_gestor.
  ENDIF.

  CASE tipo.
    WHEN 1.
*   "// Seleciona Tudo

    WHEN 2.
*   "// Seleciona a Uniorgs do Solicitante

      SELECT 'I'   AS sign,
             'EQ'  AS option,
             cod_uniorg AS low
      FROM zhcmt_pa_0031
          INTO TABLE @r_orgeh
        WHERE matricula EQ @pernr_solicitante.

      CHECK sy-subrc IS INITIAL.

    WHEN 3.
*   "// Seleciona a Area de Folha do Solicitante

      SELECT  'I'   AS sign,
              'EQ'  AS option,
              abkrs AS low
        FROM pa0001
        INTO TABLE @r_abkrs
      WHERE pernr EQ @pernr_solicitante
        AND endda >= @sy-datum.

      CHECK sy-subrc IS INITIAL.

      LOOP AT r_abkrs.
        IF r_abkrs-low = '10'.
          r_abkrs-sign = 'I'.
          r_abkrs-option = 'EQ'.
          r_abkrs-low = 11.
          APPEND r_abkrs.
          EXIT.
        ELSE.

          IF r_abkrs-low = '11'.
            r_abkrs-sign = 'I'.
            r_abkrs-option = 'EQ'.
            r_abkrs-low = 10.
            APPEND r_abkrs.
            EXIT.
          ENDIF.

          IF r_abkrs-low = '01'.
            r_abkrs-sign = 'I'.
            r_abkrs-option = 'EQ'.
            r_abkrs-low = 'A2'.
            APPEND r_abkrs.
            EXIT.
          ENDIF.
*** US - 179213 - CBRAND - Inicio
          IF r_abkrs-low = '04'.
            r_abkrs-sign = 'I'.
            r_abkrs-option = 'EQ'.
            r_abkrs-low = '17'.
            APPEND r_abkrs.
            EXIT.
          ENDIF.

          IF r_abkrs-low = '17'.
            r_abkrs-sign = 'I'.
            r_abkrs-option = 'EQ'.
            r_abkrs-low = '04'.
            APPEND r_abkrs.
            EXIT.
          ENDIF.
*** US - 179213 - CBRAND - Fim
        ENDIF.
      ENDLOOP.

* BUG - 100707 - Inicio - CBRAND
*      SELECT  'I'   AS sign,
*              'EQ'  AS option,
*              orgeh AS low
*        FROM pa0001
*        INTO TABLE @r_orgeh
*      WHERE bukrs IN @r_abkrs
*        AND endda >= @sy-datum
*        AND plans NE '99999999'.
* BUG - 100707 - Fim - CBRAND
  ENDCASE.

  SELECT *
    FROM zhcmt_f_uniorg
  INTO TABLE t_zhcmt_f_uniorg
    WHERE orgeh        IN r_orgeh
      AND area_folha   IN r_abkrs
      AND cpf_gestor   IN r_cpf_gestor
      AND nome_gestor  IN r_nome_gestor
      AND pernr_gestor IN r_pernr_gestor.

  "Passando informação para saida.
  IF t_zhcmt_f_uniorg IS NOT INITIAL.
    LOOP AT t_zhcmt_f_uniorg INTO DATA(ws_uniorg).
      APPEND VALUE #(
      cod_uniorg     =  ws_uniorg-orgeh
      area_folha     =  ws_uniorg-area_folha
      cod_ccusto     =  ws_uniorg-cod_ccusto
      cod_filial     =  ws_uniorg-cod_filial
      cod_empresa    =  ws_uniorg-cod_empresa
      nome_uniorg    =  ws_uniorg-stext
      pernr_gestor   =  ws_uniorg-pernr_gestor
      cpf_gestor     =  ws_uniorg-cpf_gestor
      nome_gestor    =  ws_uniorg-nome_gestor
      email_gestor   =  ws_uniorg-email_gestor
      nome_ccusto    =  ws_uniorg-nome_ccusto
      nome_filial    =  ws_uniorg-nome_filial
      nome_empresa   =  ws_uniorg-nome_empresa ) TO t_saida.
      CLEAR: ws_uniorg.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
