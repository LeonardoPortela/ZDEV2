*&---------------------------------------------------------------------*
*&  Include           ZSDR0082_PAI
*&---------------------------------------------------------------------*

MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.

  RANGES: LRA_MATNR FOR ZSDT0155-MATNR,
          LRA_MATKL FOR ZSDT0155-MATKL.

  DATA: LIT_MARA TYPE TABLE OF MARA.


  CASE SY-UCOMM.
    WHEN 'CONFIRM'.
      IF ( ZSDT0155-BUKRS IS INITIAL ).
        MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-AUART IS INITIAL  AND ZSDT0155-LANC_ZNFW IS INITIAL  ) OR ( ZSDT0155-AUART IS NOT INITIAL  AND ZSDT0155-LANC_ZNFW IS NOT INITIAL ).
        MESSAGE 'Favor preencher Tp.OV ou Lanc. ZNFW.' TYPE 'S'.
        EXIT.
      ENDIF.

*      IF ( ZSDT0155-AUART IS not INITIAL  and ZSDT0155-LANC_ZNFW IS not INITIAL  ).
*        MESSAGE 'É permit!' TYPE 'S'.
*        EXIT.
*      ENDIF.

      SELECT SINGLE *
        FROM TVAK INTO @DATA(_WL_TVAK)
       WHERE AUART = @ZSDT0155-AUART.

      IF SY-SUBRC NE 0 AND ZSDT0155-LANC_ZNFW IS INITIAL .
        MESSAGE 'Tipo de OV inválido!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-TP_TRIB IS INITIAL ).
        MESSAGE 'Tributo é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-DT_INI IS INITIAL ).
        MESSAGE 'Data validade inicial é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-DT_INI > SY-DATUM ).
        MESSAGE 'Data Inicial não pode ser maior que a data atual!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-DT_FIM IS INITIAL ).
        MESSAGE 'Data validade final é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-DT_INI > ZSDT0155-DT_FIM ).
        MESSAGE 'Data validade inicial maior que data de validade final!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-REGION  IS INITIAL ).
        MESSAGE 'Estado Tributação é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-MATKL IS INITIAL ) AND ( ZSDT0155-MATNR IS INITIAL  ).
        MESSAGE 'Informe Grupo Merc. ou Material!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-MATKL IS NOT INITIAL ) AND ( ZSDT0155-MATNR IS NOT INITIAL  ).
        MESSAGE 'Informe Grupo Merc. ou Material!' TYPE 'S'.
        EXIT.
      ENDIF.


*      IF ( ZSDT0155-CALC_QTE_PER IS INITIAL  AND ZSDT0155-CALC_QTE IS NOT INITIAL  ) OR ( ZSDT0155-CALC_QTE_PER IS NOT INITIAL  AND ZSDT0155-CALC_QTE IS INITIAL ).
*        MESSAGE 'Favor preencher todas as opções de cálculo de valor!' TYPE 'S'.
*        EXIT.
*      ENDIF.
*
*      IF ( ZSDT0155-CALC_QTE_PER IS INITIAL  AND ZSDT0155-VLR_UPF IS INITIAL  ) OR ( ZSDT0155-CALC_QTE_PER IS NOT INITIAL  AND ZSDT0155-VLR_UPF IS NOT INITIAL ).
*        MESSAGE 'Favor informar somente o Percentual UPF.' TYPE 'S'.
*        EXIT.
*      ENDIF.



      IF ( ZSDT0155-CALC_VLR_PER IS NOT INITIAL  AND ZSDT0155-VLR_UPF IS NOT INITIAL ).
        MESSAGE 'Favor informar somente o Percentual UPF.' TYPE 'S'.
        EXIT.
      ELSEIF ( ZSDT0155-CALC_VLR_PER IS INITIAL  AND ZSDT0155-VLR_UPF IS INITIAL ).
        MESSAGE 'Valor UPF é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-PERC_UPF IS INITIAL ).
        MESSAGE 'Percentual UPF é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-HKONT_CRED IS INITIAL ).
        MESSAGE 'Conta Crédito é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZSDT0155-HKONT_DEB IS INITIAL ).
        MESSAGE 'Conta Débito é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      ZSDT0155-CANCELADO = ''.

      CLEAR: LRA_MATNR[], LRA_MATKL[], LIT_MARA[].

      IF ZSDT0155-MATKL IS NOT INITIAL.

        APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = ZSDT0155-MATKL ) TO LRA_MATKL.

        SELECT *
          FROM MARA INTO TABLE LIT_MARA
         WHERE MATKL EQ ZSDT0155-MATKL.

        LOOP AT LIT_MARA INTO DATA(LWA_MARA).
          APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LWA_MARA-MATNR ) TO LRA_MATNR.
        ENDLOOP.

        IF ( LIT_MARA[] IS INITIAL ).
          MESSAGE |Não encontrado materiais para o grupo de mercadoria { ZSDT0155-MATKL }!| TYPE 'S'.
          RETURN.
        ENDIF.

      ELSEIF ZSDT0155-MATNR IS NOT INITIAL.

        APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = ZSDT0155-MATNR ) TO LRA_MATNR.

        SELECT SINGLE *
          FROM MARA INTO @DATA(LWA_MARA_CK)
         WHERE MATNR EQ @ZSDT0155-MATNR.

        IF SY-SUBRC NE 0.
          MESSAGE |Material { ZSDT0155-MATNR } não encontrado!| TYPE 'S'.
          RETURN.
        ENDIF.

        IF LWA_MARA_CK-MATKL IS INITIAL.
          MESSAGE |Grupo Merc. Material { ZSDT0155-MATNR } não encontrado!| TYPE 'S'.
          RETURN.
        ENDIF.

        APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LWA_MARA_CK-MATKL ) TO LRA_MATKL.

      ELSE.
        EXIT.
      ENDIF.

      IF VG_OPERACAO = C_NOVO.

        SELECT SINGLE *
          FROM ZSDT0155 INTO @DATA(_WL_0155)
         WHERE BUKRS     EQ @ZSDT0155-BUKRS
           AND AUART     EQ @ZSDT0155-AUART
           AND LANC_ZNFW EQ @ZSDT0155-LANC_ZNFW
           AND TP_TRIB   EQ @ZSDT0155-TP_TRIB
           AND ( ( MATKL IN @LRA_MATKL ) OR
                 ( MATNR IN @LRA_MATNR ) )
           AND DT_INI    EQ @ZSDT0155-DT_INI.

        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro para essa Empresa/Tp.OV./Lcto.ZNFW/Tributo/Grupo.Merc/Material/Dt.Ini!' TYPE 'S'.
          EXIT.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja realmente gravar o registro?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = VAR_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      CHECK VAR_ANSWER EQ '1'.

      ZSDT0155-DT_REGISTRO = SY-DATUM.
      ZSDT0155-HR_REGISTRO = SY-UZEIT.
      ZSDT0155-US_REGISTRO = SY-UNAME.

      MODIFY ZSDT0155 FROM ZSDT0155.

      IF SY-SUBRC = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.

        "Anula parametros anteriores
        SELECT *
          FROM ZSDT0155 INTO TABLE @DATA(_TG_0155)
         WHERE BUKRS     EQ @ZSDT0155-BUKRS
           AND AUART     EQ @ZSDT0155-AUART
           AND LANC_ZNFW EQ @ZSDT0155-LANC_ZNFW
           AND TP_TRIB   EQ @ZSDT0155-TP_TRIB
           AND ( ( MATKL IN @LRA_MATKL ) OR
                 ( MATNR IN @LRA_MATNR ) )
           AND DT_INI    NE @ZSDT0155-DT_INI.

        DELETE _TG_0155 WHERE CANCELADO = ABAP_TRUE.

        IF LINES( _TG_0155[] ) > 0.
          MESSAGE | { LINES( _TG_0155[] ) } parâmetro(s) será(ão) desativado(s)! | TYPE 'I'.
        ENDIF.

        LOOP AT _TG_0155 INTO _WL_0155.
          _WL_0155-CANCELADO = 'X'.
          MODIFY ZSDT0155 FROM _WL_0155.
        ENDLOOP.

        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
      ENDIF.


    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
