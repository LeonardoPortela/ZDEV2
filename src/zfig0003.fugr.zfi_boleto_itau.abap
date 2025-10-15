FUNCTION ZFI_BOLETO_ITAU.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_BOLETOS TYPE  ZDE_BOLETO_ITAU_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: R_NOSSO_NUMERO TYPE RANGE OF CHAR15.

* "// Armazenamento de LOG de Entrada
  INCLUDE ZAFL_MACROS.
  /AFL/LOG_INIT.
  /AFL/SAVE.

  DATA: LS_ITAU_BOLETO TYPE ZDE_IN_ITAU_BOLETO.

  LOOP AT T_BOLETOS INTO DATA(LS_BOLETOS).

    IF LS_BOLETOS-IDBENEFICIARIO IS INITIAL.
      MESSAGE 'Id do Beneficiario é Obrigatorio!' TYPE 'E'.
    ENDIF.

    IF LS_BOLETOS-NUMERONOSSONUMERO IS INITIAL.
      MESSAGE 'O Nosso Numero é Obrigatorio!' TYPE 'E'.
    ENDIF.

*"// Indicador do tipo de liquidação.
*"// 06-Liquidação normal, 95-Baixa Operacional
    IF LS_BOLETOS-TIPOLIQUIDACAO EQ '06'.
      DATA(LV_LIQUIDADO) = ABAP_TRUE.
    ENDIF.

    LS_ITAU_BOLETO =
    VALUE #(
             ID_BENEFICIARIO = LS_BOLETOS-IDBENEFICIARIO
             NOSSO_NUMERO = LS_BOLETOS-NUMERONOSSONUMERO
             CORRELATIONID = /AFL/LOG-GUID
             CONTROLE = SY-XFORM
           ).

    APPEND
    VALUE #(
              SIGN   = 'I'
              OPTION = 'EQ'
              LOW    = |{ LS_BOLETOS-NUMERONOSSONUMERO ALPHA = OUT }|
              HIGH   = |{ LS_BOLETOS-NUMERONOSSONUMERO ALPHA = OUT }|
           ) TO R_NOSSO_NUMERO.

    CALL METHOD ZCL_FI_UTILS=>RUN_API_ITAU
      EXPORTING
        I_ITAU_BOLETO = LS_ITAU_BOLETO
        IS_PIX        = LV_LIQUIDADO.

    CLEAR LS_ITAU_BOLETO.

  ENDLOOP.

*"// Verifica se o Nosso Numero é da AMAGGION
*"// caso não for é excluido para não seguir o fluxo
  SELECT *
    FROM ZFIT0225
      INTO TABLE @DATA(LT_ZFIT0225)
    WHERE NOSSO_NUMERO IN @R_NOSSO_NUMERO.

  DATA(T_BOLETOS_AUX) = T_BOLETOS[].

  LOOP AT T_BOLETOS_AUX ASSIGNING FIELD-SYMBOL(<FS_BOLETOS_AUX>).
    READ TABLE LT_ZFIT0225 TRANSPORTING NO FIELDS WITH KEY NOSSO_NUMERO = <FS_BOLETOS_AUX>-NUMERONOSSONUMERO.
    IF SY-SUBRC IS NOT INITIAL.
      CLEAR <FS_BOLETOS_AUX>.
    ENDIF.
  ENDLOOP.

  DELETE T_BOLETOS_AUX WHERE NUMERONOSSONUMERO IS INITIAL.

  T_BOLETOS[] = T_BOLETOS_AUX[].

ENDFUNCTION.
