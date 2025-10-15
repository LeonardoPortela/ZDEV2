
MODULE USER_COMMAND_0100 INPUT.
  O_ALV->REFRESH( ).
  PERFORM ACTION_PROCESS.
ENDMODULE.

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'T0100'.
  CREATE OBJECT LO_REPORT.
  "lo_report->get_data( ).
  LO_REPORT->GENERATE_OUTPUT( ).
  LO_REPORT->SET_REFRESH( ).
ENDMODULE.

FORM ACTION_PROCESS.
*  DATA: retorn_msg TYPE char72.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'SAVE'. "GRAVAR
*       DATA: retorn_msg TYPE char72.
      CLEAR: RETORN_MSG.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          FUNCTIONCODE           = '=ENT'
        EXCEPTIONS
          FUNCTION_NOT_SUPPORTED = 1
          OTHERS                 = 2.

      CLEAR:WA_SAIDA.
      IF IT_SAIDA IS NOT INITIAL.
        LOOP AT IT_SAIDA INTO WA_SAIDA.
          IF WA_SAIDA-CH_MODELO IS NOT INITIAL. "Opção copiar.

            "Verifica se ja existe o cadastro com dados
            SELECT SINGLE * FROM ZSDT0370 INTO @DATA(IT_ZSDT0370) WHERE AUART = @WA_SAIDA-AUART
            AND VKAUS         = @WA_SAIDA-VKAUS
            AND  BRSCH        = @WA_SAIDA-BRSCH
            AND  UF_CENTRO    = @WA_SAIDA-UF_CENTRO
            AND  UF_CLIENTE   = @WA_SAIDA-UF_CLIENTE
            AND  CITYC        = @WA_SAIDA-CITYC
            AND  MWSK1        = @WA_SAIDA-MWSK1
            AND  OWNPR        = @WA_SAIDA-OWNPR
            AND  SHTYP        = @WA_SAIDA-SHTYP
            AND  TDLNR        = @WA_SAIDA-TDLNR
            AND  BUKRS_TOMA   = @WA_SAIDA-BUKRS_TOMA
            AND  BUKRS_EMIT   = @WA_SAIDA-BUKRS_EMIT
            AND  MTORG        = @WA_SAIDA-MTORG
            AND  MATNR        = @WA_SAIDA-MATNR
            AND  MATKL        = @WA_SAIDA-MATKL
            AND  EXTWG        = @WA_SAIDA-EXTWG
            AND  STEUC        = @WA_SAIDA-STEUC
            AND  J_1BTXSDC    = @WA_SAIDA-J_1BTXSDC
            AND  J_1BTAXLW1   = @WA_SAIDA-J_1BTAXLW1
            AND  J_1BTAXLW2   = @WA_SAIDA-J_1BTAXLW2
            AND  J_1BTAXLW4   = @WA_SAIDA-J_1BTAXLW4
            AND  J_1BTAXLW5   = @WA_SAIDA-J_1BTAXLW5.
*            select single * from zsdt0370 where ch_modelo = @wa_saida-ch_modelo into @data(wa_valida1).
            "Se encontrar informações na tabela retornar msg informando que ja exisite o cadastro.
            IF SY-SUBRC EQ 0.
              RETORN_MSG = 'Ja existe cadastro para os dados informado'.
              MESSAGE  RETORN_MSG TYPE 'I'.
            ELSE.
              "Pegar um nova sequencia.
              TRY.
                  WA_SAIDA-CH_MODELO = ZCL_IMPOSTOS=>GET_CH_MODELO( ).
                CATCH ZCX_ERROR INTO DATA(EX_ERROR).
                  MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO DATA(L_MESG).
              ENDTRY.

              CLEAR: WA_ZSDT0370.

              MOVE-CORRESPONDING WA_SAIDA TO WA_ZSDT0370.

              MODIFY ZSDT0370 FROM WA_ZSDT0370.
              COMMIT WORK.
            ENDIF.

          ELSE.
            "Opção inserir.
            SELECT SINGLE * FROM ZSDT0370
            WHERE 1 = 1
            AND AUART = @WA_SAIDA-AUART
            AND VKAUS = @WA_SAIDA-VKAUS
            AND BRSCH = @WA_SAIDA-BRSCH
            AND UF_CENTRO = @WA_SAIDA-UF_CENTRO
            AND UF_CLIENTE = @WA_SAIDA-UF_CLIENTE
            AND CITYC = @WA_SAIDA-CITYC
            AND MWSK1 = @WA_SAIDA-MWSK1
            AND OWNPR = @WA_SAIDA-OWNPR
            AND SHTYP = @WA_SAIDA-SHTYP
            AND TDLNR = @WA_SAIDA-TDLNR
            AND BUKRS_TOMA = @WA_SAIDA-BUKRS_TOMA
            AND BUKRS_EMIT = @WA_SAIDA-BUKRS_EMIT
*            and empresa = @wa_saida-empresa
            AND MTORG = @WA_SAIDA-MTORG
            AND MATNR = @WA_SAIDA-MATNR
            AND MATKL = @WA_SAIDA-MATKL
            AND EXTWG = @WA_SAIDA-EXTWG
            AND STEUC = @WA_SAIDA-STEUC
            AND J_1BTXSDC = @WA_SAIDA-J_1BTXSDC
            AND J_1BTAXLW1 = @WA_SAIDA-J_1BTAXLW1
            AND J_1BTAXLW2 = @WA_SAIDA-J_1BTAXLW2
            AND J_1BTAXLW4 = @WA_SAIDA-J_1BTAXLW4
            AND J_1BTAXLW5 = @WA_SAIDA-J_1BTAXLW5
            INTO @DATA(WA_VALIDA2).

            IF SY-SUBRC = 0.
              CLEAR: WA_ZSDT0370.
              MOVE-CORRESPONDING WA_SAIDA TO WA_ZSDT0370.
              MODIFY ZSDT0370 FROM WA_ZSDT0370.
              COMMIT WORK.
            ELSE.

              TRY.
                  WA_SAIDA-CH_MODELO = ZCL_IMPOSTOS=>GET_CH_MODELO( ).
                CATCH ZCX_ERROR INTO EX_ERROR.
                  MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO L_MESG.
              ENDTRY.
              CLEAR: WA_ZSDT0370.
              MOVE-CORRESPONDING WA_SAIDA TO WA_ZSDT0370.
              INSERT ZSDT0370 FROM WA_ZSDT0370.
              COMMIT WORK.
            ENDIF.

          ENDIF.
        ENDLOOP.


        IF RETORN_MSG IS NOT INITIAL.
*          message e024(sd) with retorn_msg.
          MESSAGE  RETORN_MSG TYPE 'E'.
          EXIT.
        ELSE.
          FREE: IT_SAIDA.
          CLEAR:WA_SAIDA.
          LEAVE PROGRAM.
        ENDIF.
      ELSE.
        MESSAGE 'Não Existem Dados a Serem Salvos!' TYPE 'I'.
      ENDIF.

  ENDCASE.
ENDFORM.
FORM GRAVAR.


  IF IT_SAIDA IS NOT INITIAL.

    TYPE-POOLS: ESP1.

    DATA: LT_TAB TYPE ESP1_MESSAGE_TAB_TYPE.
    DATA: LS_TAB TYPE ESP1_MESSAGE_WA_TYPE.
    DATA: IT_ERRO TYPE STANDARD TABLE OF STRING INITIAL SIZE 0.
    DATA: WA_ERRO TYPE STRING.
    DATA: LINHA TYPE I.

    FREE:LT_TAB, IT_ERRO.

    LOOP AT IT_SAIDA INTO WA_SAIDA.

      CLEAR: LINHA.
      LINHA = SY-TABIX.

      "Tipo Doc. Vendas.
      IF WA_SAIDA-AUART IS NOT INITIAL.
        SELECT SINGLE * FROM TVAKT INTO @DATA(WA_TVAKT)
          WHERE AUART EQ @WA_SAIDA-AUART AND SPRAS EQ @SY-LANGU.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Tipo de ordem não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
""Retorno de homologação key-user- parte 1 BUG #181721 - BG inicio
        "obriga a informar ao menos o tipo de venda para evitar de informar uma linha em branco
        else.
           CLEAR: WA_ERRO.
          WA_ERRO = |Tipo de ordem não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
""Retorno de homologação key-user- parte 1 BUG #181721 - BG Fim
      ENDIF.

      "Empresa emitente
      IF WA_SAIDA-BUKRS_EMIT IS NOT INITIAL.
        SELECT SINGLE * FROM T001 INTO @DATA(WA_T001)
          WHERE BUKRS EQ @WA_SAIDA-BUKRS_EMIT.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Empresa emitente não cadastrada!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Empresa tomadora
      IF WA_SAIDA-BUKRS_TOMA IS NOT INITIAL.
        SELECT SINGLE * FROM T001 INTO WA_T001
          WHERE BUKRS EQ WA_SAIDA-BUKRS_TOMA.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Empresa tomadora não cadastrada!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Material
      IF WA_SAIDA-MATNR IS NOT INITIAL.
        SELECT SINGLE * FROM MARA INTO @DATA(WA_MARA)
          WHERE MATNR EQ @WA_SAIDA-MATNR.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Material não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      "Grup. Merc.
      IF WA_SAIDA-MATKL IS NOT INITIAL.
        SELECT SINGLE * FROM MARA INTO WA_MARA
          WHERE MATKL EQ WA_SAIDA-MATKL.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Grupo mercadoria não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


*      "Grup. Merc. Ext.
      IF WA_SAIDA-EXTWG IS NOT INITIAL. "Retorno de homologação key-user- parte 1 BUG #181721 - BG
        SELECT SINGLE * FROM TWEW INTO @DATA(wa_TWEW)
          WHERE EXTWG EQ @WA_SAIDA-EXTWG.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Grupo mercadoria externo não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF."Retorno de homologação key-user- parte 1 BUG #181721 - BG
      ENDIF.

      "Utilização.
      IF WA_SAIDA-VKAUS IS NOT INITIAL.
        SELECT SINGLE * FROM TVLV INTO @DATA(WA_TVLV)
          WHERE ABRVW EQ @WA_SAIDA-VKAUS.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Tipo de utlização não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Setor Industrial.
      IF WA_SAIDA-BRSCH IS NOT INITIAL.
        SELECT SINGLE * FROM T016 INTO @DATA(wa_T016)
          WHERE BRSCH EQ @WA_SAIDA-BRSCH.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Tipo de Setor Industrial não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      "UF Receptor.
      IF WA_SAIDA-UF_CENTRO IS NOT INITIAL.
        SELECT SINGLE * FROM T005U INTO @DATA(wa_T005U)
          WHERE BLAND EQ @WA_SAIDA-UF_CENTRO
          AND SPRAS EQ @SY-LANGU
            AND LAND1 EQ 'BR'.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |UF Emissor não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      "UF Receptor.
      IF WA_SAIDA-UF_CLIENTE IS NOT INITIAL.
        SELECT SINGLE * FROM T005U INTO wa_T005U
          WHERE BLAND EQ WA_SAIDA-UF_CLIENTE
            AND SPRAS EQ SY-LANGU
            AND LAND1 EQ 'BR'.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |UF Receptor não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

*      "Código da Cidade.
*      if wa_saida-cityc is not initial.
*        select single * from j_1btreg_city into @data(wa_J_1BTREG_CITY)
*          where taxjurcode eq @wa_saida-cityc
*            and country eq 'BR'.
*        if sy-subrc ne 0.
*          clear: wa_erro.
*          wa_erro = |Código da Cidade não cadastrado!|.
*          append wa_erro to it_erro.
*          continue.
*        endif.
*      endif.


      "Tp. Transp.
      IF WA_SAIDA-SHTYP IS NOT INITIAL.
        SELECT SINGLE * FROM TVTK INTO @DATA(WA_TVTK)
          WHERE SHTYP EQ @WA_SAIDA-SHTYP.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Tp. Transp. não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      "IVA.
      IF WA_SAIDA-J_1BTXSDC IS NOT INITIAL.
        SELECT SINGLE * FROM J_1BTXSDCT INTO @DATA(WA_J_1BTXSDCT)
          WHERE TAXCODE EQ @WA_SAIDA-J_1BTXSDC AND LANGU EQ @SY-LANGU.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |IVA não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Txt.IPI.
      IF WA_SAIDA-J_1BTAXLW2 IS NOT INITIAL.
        SELECT SINGLE * FROM J_1BATL2 INTO @DATA(wa_J_1BATL2)
          WHERE TAXLAW EQ @WA_SAIDA-J_1BTAXLW2.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |IPI não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Txt.ICMS.
      IF WA_SAIDA-J_1BTAXLW1 IS NOT INITIAL.
        SELECT SINGLE * FROM  J_1BATL1 INTO @DATA(wa_J_1BATL1)
          WHERE TAXLAW EQ @WA_SAIDA-J_1BTAXLW1.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |ICMS não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      "Lei COFINS.
      IF WA_SAIDA-J_1BTAXLW4 IS NOT INITIAL.
        SELECT SINGLE * FROM J_1BATL4A INTO @DATA(wa_J_1BATL4A)
          WHERE TAXLAW EQ @WA_SAIDA-J_1BTAXLW4.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Lei COFINS não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.

      "Lei PIS.
      IF WA_SAIDA-J_1BTAXLW5 IS NOT INITIAL.
        SELECT SINGLE * FROM J_1BATL5 INTO @DATA(wa_J_1BATL5)
          WHERE TAXLAW EQ @WA_SAIDA-J_1BTAXLW5.
        IF SY-SUBRC NE 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Lei PIS não cadastrado!|.
          APPEND WA_ERRO TO IT_ERRO.
          CONTINUE.
        ENDIF.
      ENDIF.


      IF WA_SAIDA-CH_MODELO IS NOT INITIAL. "Opção copiar.

        "Verifica se ja existe o cadastro com dados
        CLEAR: WA_ZSDT0370.
        SELECT SINGLE * FROM ZSDT0370 INTO WA_ZSDT0370 WHERE AUART = WA_SAIDA-AUART
        AND VKAUS         = WA_SAIDA-VKAUS
        AND  BRSCH        = WA_SAIDA-BRSCH
        AND  UF_CENTRO    = WA_SAIDA-UF_CENTRO
        AND  UF_CLIENTE   = WA_SAIDA-UF_CLIENTE
        AND  CITYC        = WA_SAIDA-CITYC
        AND  MWSK1        = WA_SAIDA-MWSK1
        AND  OWNPR        = WA_SAIDA-OWNPR
        AND  SHTYP        = WA_SAIDA-SHTYP
        AND  TDLNR        = WA_SAIDA-TDLNR
        AND  BUKRS_TOMA   = WA_SAIDA-BUKRS_TOMA
        AND  BUKRS_EMIT   = WA_SAIDA-BUKRS_EMIT
        AND  MTORG        = WA_SAIDA-MTORG
        AND  MATNR        = WA_SAIDA-MATNR
        AND  MATKL        = WA_SAIDA-MATKL
        AND  EXTWG        = WA_SAIDA-EXTWG
        AND  STEUC        = WA_SAIDA-STEUC
        AND  J_1BTXSDC    = WA_SAIDA-J_1BTXSDC
        AND  J_1BTAXLW1   = WA_SAIDA-J_1BTAXLW1
        AND  J_1BTAXLW2   = WA_SAIDA-J_1BTAXLW2
        AND  J_1BTAXLW4   = WA_SAIDA-J_1BTAXLW4
        AND  J_1BTAXLW5   = WA_SAIDA-J_1BTAXLW5.
*            select single * from zsdt0370 where ch_modelo = @wa_saida-ch_modelo into @data(wa_valida1).
        "Se encontrar informações na tabela retornar msg informando que ja exisite o cadastro.
        IF SY-SUBRC EQ 0.
          CLEAR: WA_ERRO.
          WA_ERRO = |Dados informados já foram cadastrados!|. "Retorno de homologação key-user- parte 1 BUG #181721 - BG mudei a msg para não cortar na saida
          APPEND WA_ERRO TO IT_ERRO.
        ELSE.
          "Pegar um nova sequencia.
          TRY.
              WA_SAIDA-CH_MODELO = ZCL_IMPOSTOS=>GET_CH_MODELO( ).
            CATCH ZCX_ERROR INTO DATA(EX_ERROR).
              MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO DATA(L_MESG).
          ENDTRY.

          CLEAR: WA_ZSDT0370.

          MOVE-CORRESPONDING WA_SAIDA TO WA_ZSDT0370.

          MODIFY ZSDT0370 FROM WA_ZSDT0370.
          COMMIT WORK.
        ENDIF.

      ELSE.
        "Opção inserir.
        SELECT SINGLE * FROM ZSDT0370
        WHERE 1 = 1
        AND AUART = @WA_SAIDA-AUART
        AND VKAUS = @WA_SAIDA-VKAUS
        AND BRSCH = @WA_SAIDA-BRSCH
        AND UF_CENTRO = @WA_SAIDA-UF_CENTRO
        AND UF_CLIENTE = @WA_SAIDA-UF_CLIENTE
        AND CITYC = @WA_SAIDA-CITYC
        AND MWSK1 = @WA_SAIDA-MWSK1
        AND OWNPR = @WA_SAIDA-OWNPR
        AND SHTYP = @WA_SAIDA-SHTYP
        AND TDLNR = @WA_SAIDA-TDLNR
        AND BUKRS_TOMA = @WA_SAIDA-BUKRS_TOMA
        AND BUKRS_EMIT = @WA_SAIDA-BUKRS_EMIT
*            and empresa = @wa_saida-empresa
        AND MTORG = @WA_SAIDA-MTORG
        AND MATNR = @WA_SAIDA-MATNR
        AND MATKL = @WA_SAIDA-MATKL
        AND EXTWG = @WA_SAIDA-EXTWG
        AND STEUC = @WA_SAIDA-STEUC
        AND J_1BTXSDC = @WA_SAIDA-J_1BTXSDC
        AND J_1BTAXLW1 = @WA_SAIDA-J_1BTAXLW1
        AND J_1BTAXLW2 = @WA_SAIDA-J_1BTAXLW2
        AND J_1BTAXLW4 = @WA_SAIDA-J_1BTAXLW4
        AND J_1BTAXLW5 = @WA_SAIDA-J_1BTAXLW5
        INTO @DATA(WA_VALIDA2).

        IF SY-SUBRC = 0.
          CLEAR: WA_ZSDT0370.
""Retorno de homologação key-user- parte 1 BUG #181721 - BG - INICIO
*          MOVE-CORRESPONDING: WA_SAIDA TO WA_ZSDT0370.
*          MODIFY ZSDT0370 FROM WA_ZSDT0370.
*          COMMIT WORK.
          CLEAR: WA_ERRO.
          WA_ERRO = |Dados informados já foram cadastrados!|.
          APPEND WA_ERRO TO IT_ERRO.
""Retorno de homologação key-user- parte 1 BUG #181721 - BG - FIM
        ELSE.

          TRY.
              WA_SAIDA-CH_MODELO = ZCL_IMPOSTOS=>GET_CH_MODELO( ).
            CATCH ZCX_ERROR INTO EX_ERROR.
              MESSAGE ID EX_ERROR->MSGID TYPE 'S' NUMBER EX_ERROR->MSGNO WITH EX_ERROR->MSGV1 EX_ERROR->MSGV2 EX_ERROR->MSGV3 EX_ERROR->MSGV4 INTO L_MESG.
          ENDTRY.
          CLEAR: WA_ZSDT0370.
          MOVE-CORRESPONDING WA_SAIDA TO WA_ZSDT0370.
          INSERT ZSDT0370 FROM WA_ZSDT0370.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF IT_ERRO IS NOT INITIAL.
      LOOP AT IT_ERRO ASSIGNING FIELD-SYMBOL(<FS_ERRO>).
        LS_TAB-MSGID  = 'E4'.
        LS_TAB-MSGNO  = '000'.
        LS_TAB-MSGTY  = 'E'.
        LS_TAB-MSGV1  = |Linha { LINHA } - { <FS_ERRO> }|.
        LS_TAB-LINENO = SY-TABIX.
        APPEND LS_TAB TO LT_TAB.
        CLEAR: LS_TAB.
      ENDLOOP.
    ENDIF.

    IF LT_TAB IS NOT INITIAL.
      SORT LT_TAB.

      DELETE ADJACENT DUPLICATES FROM LT_TAB.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          I_MESSAGE_TAB = LT_TAB.
    ELSE.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.
