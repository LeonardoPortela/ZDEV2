function Z_SOL_OV_ESTRAT_EXECUTA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"     REFERENCE(I_MOTIVO) TYPE  CHAR50 OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"     VALUE(OK) TYPE  CHAR01
*"  TABLES
*"      T_SOLICITACOES STRUCTURE  ZSDS018
*"      T_ESTRA STRUCTURE  ZSDS019
*"----------------------------------------------------------------------

  type-pools: ICON.

  types:

    begin of TY_ESTRA,
      BUKRS      type ZSDT0162-BUKRS,
      NRO_SOL_OV type ZSDT0162-VBELN,
      VALOR_DE   type ZSDT0161-VALOR_DE,
      VALOR_ATE  type ZSDT0161-VALOR_ATE,
      APROVADOR  type ZSDT0161-APROVADOR,
      NIVEL      type ZSDT0161-NIVEL,
      WAERS(3),
      ESTADO(4),
      OPCOES(4),
    end of TY_ESTRA.

  data: TG_ESTRA       type table of TY_ESTRA,
        TL_TEXTO       type CATSXT_LONGTEXT_ITAB,

        WG_ESTRA       like line of TG_ESTRA,
        WG_ESTRA2      like line of TG_ESTRA,
        WG_INPUT_ESTRA type ZSDT0162,
        V_VALIDO       type C,
        W_ESTRA        type ZSDS019.

  data: FLAG_UNDO(1),
        LINHA_ESTRA   type SY-TABIX,
        ULT_LINHA     type SY-TABIX,
        E_ROW_ID      type SY-TABIX,
        VL_NUM_LOG_ID type CHAR10.

  data: OBJ_ZCL_WEBSERVICE_TAXA_CURVA type ref to ZCL_WEBSERVICE_TX_CURVA.
  data: CX_EXCEPTION type ref to ZCX_WEBSERVICE.

  OK = ABAP_FALSE.

  loop at T_ESTRA.
    move-corresponding T_ESTRA to WG_ESTRA.
    append WG_ESTRA to TG_ESTRA.
  endloop.

  sort TG_ESTRA by NIVEL APROVADOR.
  sort T_ESTRA  by NIVEL APROVADOR.

  loop at TG_ESTRA into WG_ESTRA where APROVADOR eq V_USUARIO.
    data(VL_CHECK_NIVEL) = WG_ESTRA-NIVEL + 1.

    read table T_SOLICITACOES into data(WL_SOLICITACOES) index 1.
    select single *
      from ZSDT0162
      into @data(WL_CHECK_ZSDT0162)
      where VBELN eq @WL_SOLICITACOES-NRO_SOL_OV
        and NIVEL eq @VL_CHECK_NIVEL
        and CK_RECUSA eq ''.

    if ( SY-SUBRC eq 0 ).
      message 'Solicitação já aprovada em nível superior!' type 'I'.
      exit.
    endif.

    if E_ROW_ID is not initial and SY-TABIX ne E_ROW_ID + 1.
      exit.
    elseif E_ROW_ID is not initial and SY-TABIX eq E_ROW_ID + 1.
      if WG_ESTRA-OPCOES ne ICON_SYSTEM_UNDO.
        move ICON_SET_STATE to WG_ESTRA-OPCOES.
      endif.
      E_ROW_ID = SY-TABIX.
    else.
      E_ROW_ID = SY-TABIX.
    endif.

    read table T_SOLICITACOES index 1.

    if WG_ESTRA-OPCOES ne ICON_SET_STATE and WG_ESTRA-OPCOES ne ICON_SYSTEM_UNDO and WG_ESTRA-OPCOES ne ICON_REJECT .
      MSG =  'Opção inválida para processamento!'.
      exit.
    endif.

    if V_USUARIO ne WG_ESTRA-APROVADOR.
      MSG =  'Usuário não é o aprovador deste nível!'.
      exit.
    endif.

    if  WG_ESTRA-OPCOES = ICON_SET_STATE.
      FLAG_UNDO = 'S'.
      LINHA_ESTRA =  E_ROW_ID.

      loop at TG_ESTRA into WG_ESTRA2.
        ULT_LINHA = SY-TABIX.
        if WG_ESTRA2-OPCOES = ICON_SET_STATE and SY-TABIX lt E_ROW_ID.
          FLAG_UNDO = 'N'.
        endif.
      endloop.

      if FLAG_UNDO eq 'S'.

        call function 'NUMBER_GET_NEXT'
          exporting
            NR_RANGE_NR             = '01'
            OBJECT                  = 'ZSD_LOG_OV'
          importing
            NUMBER                  = VL_NUM_LOG_ID
          exceptions
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            BUFFER_OVERFLOW         = 7
            others                  = 8.


        WG_INPUT_ESTRA-ID_LOG     = VL_NUM_LOG_ID.
        WG_INPUT_ESTRA-BUKRS      = WG_ESTRA-BUKRS.
        WG_INPUT_ESTRA-VBELN      = WG_ESTRA-NRO_SOL_OV.
        WG_INPUT_ESTRA-STATUS     = 'L'.
        WG_INPUT_ESTRA-NIVEL      = WG_ESTRA-NIVEL.
        WG_INPUT_ESTRA-APROVADOR  = WG_ESTRA-APROVADOR.
        WG_INPUT_ESTRA-VALOR_DE   = WG_ESTRA-VALOR_DE.
        WG_INPUT_ESTRA-VALOR_ATE  = WG_ESTRA-VALOR_ATE.
        WG_INPUT_ESTRA-DATA_ATUAL = SY-DATUM.
        WG_INPUT_ESTRA-HORA_ATUAL = SY-UZEIT.
        WG_INPUT_ESTRA-USUARIO    = SY-UNAME.

        modify ZSDT0162 from WG_INPUT_ESTRA.
        commit work.
*        CLEAR WG_INPUT_ESTRA.

        read table T_SOLICITACOES into WL_SOLICITACOES index 1.
        if ( ULT_LINHA eq LINHA_ESTRA ).

* ---> S4 Migration - 10/06/2023 - DG
          "          IF ( WG_ESTRA-VALOR_ATE GE  WL_SOLICITACOES-VALOR ).

          data: LV_VALOR_ATE type DMBTR.

          LV_VALOR_ATE = conv #( WG_ESTRA-VALOR_ATE ).

          if ( LV_VALOR_ATE ge  WL_SOLICITACOES-VALOR ).
* <--- S4 Migration - 10/06/2023 - DG

            update ZSDT0051 set STATUS = 'L' JOB = ABAP_FALSE DATA_ATUAL = SY-DATUM USNAM = SY-UNAME
            where NRO_SOL_OV eq WG_ESTRA-NRO_SOL_OV.

            select single BSTKD
              from ZSDT0051
              into @data(_BSTKD)
              where NRO_SOL_OV eq @WG_ESTRA-NRO_SOL_OV.

            if SY-SUBRC is initial.
              update ZSDT0143 set STS_SOL = 'L' where CONTRATO eq _BSTKD.
            endif.

            WG_INPUT_ESTRA-ULTIMO_NIVEL    = 'X'.
            modify ZSDT0162 from WG_INPUT_ESTRA.

            commit work.

            perform GERA_HTML_LIBERACAO(ZSDR0088) using WG_ESTRA-NRO_SOL_OV.

          endif.

          clear WG_INPUT_ESTRA.

*          IF ( SY-SUBRC EQ 0 ).
          select single * from ZSDT0051
            into @data(WL_ZSDT0051)
            where NRO_SOL_OV eq @WG_ESTRA-NRO_SOL_OV.

          if ( WL_ZSDT0051 is not initial ).

            if ( WL_ZSDT0051-PARAM_ESPEC ne 'M' ) and ( WL_ZSDT0051-STATUS eq 'L' ).

              case WL_ZSDT0051-WAERK.
                when 'BRL'.

                  select single * from SETLEAF
                    into @data(WL_SETLEAF)
                    where SETNAME eq 'MAGGI_ZSDT0062_HEDGE'
                      and VALFROM eq @WL_ZSDT0051-TP_VENDA.

                  if ( SY-SUBRC eq 0 ).
                    data: VL_TCODE type SY-TCODE,
                          VAR_MSG  type STRING.

*                   Empresa 0032 disparar somente diferente de ZCIC "Intercompany", e com o Grupo de Mercadoria 700170 700110 "Soja e Milho"
                    if WL_ZSDT0051-VKORG eq '0032'.
                      V_VALIDO = ABAP_TRUE.

                      select count(*)
                        from KNA1
                        where LIFNR eq @WL_ZSDT0051-KUNNR
                        and KTOKD eq 'ZCIC'.

                      if SY-SUBRC is not initial.

                        select count(*)
                          from MARA
                          where MATNR eq WL_ZSDT0051-MATNR
                          and MATKL in ('700110', '700170').

                        if SY-SUBRC is initial.
                          clear V_VALIDO.
                        endif.
                      endif.
                    endif.

                    VL_TCODE = 'ZSDT0062'.

                    if V_VALIDO is initial.
                      "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
                      free: OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
                      create object OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
                      try.
                          OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO =  WG_ESTRA-NRO_SOL_OV
                                                                   I_TIPO   = 'EDI'
                                                                   I_TCODE  = VL_TCODE
                                                                  ).

                          OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO =  WG_ESTRA-NRO_SOL_OV
                                                                   I_TIPO   = 'LIB'
                                                                   I_TCODE  = VL_TCODE
                                                                   ).

                        catch ZCX_WEBSERVICE into CX_EXCEPTION.
                          VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
                          message E007(ZWEBSERVICE) display like 'W' with VAR_MSG.
                      endtry.
                    endif.
                  endif.
              endcase.
            endif.
          endif.
*          ENDIF.

          loop at TG_ESTRA into WG_ESTRA.
            WG_ESTRA-OPCOES = ' ' .
            WG_ESTRA-ESTADO = ICON_CHECKED .
            modify TG_ESTRA from WG_ESTRA index SY-TABIX transporting OPCOES ESTADO.
          endloop.

          MSG = 'Processamento concluído com sucesso'.
          OK = ABAP_TRUE.

        else.
          clear WG_INPUT_ESTRA.

          WG_ESTRA-OPCOES = ICON_SYSTEM_UNDO.
          WG_ESTRA-ESTADO = ICON_CHECKED .
          modify TG_ESTRA from WG_ESTRA index E_ROW_ID.
          MSG = 'Processamento concluído com sucesso'.
          OK = ABAP_TRUE.
        endif.
      else.
        MSG = 'Devem ser aprovadas as estratégias anteriores'.
      endif.
    elseif WG_ESTRA-OPCOES = ICON_SYSTEM_UNDO .
      FLAG_UNDO = 'S'.
      LINHA_ESTRA =  E_ROW_ID.


      loop at TG_ESTRA into WG_ESTRA2.
        if WG_ESTRA2-OPCOES = ICON_SYSTEM_UNDO and SY-TABIX gt E_ROW_ID
          and WG_ESTRA2-APROVADOR ne WG_ESTRA-APROVADOR.                        "/Modificação CS2016000820/
          FLAG_UNDO = 'N'.
        endif.
        if WG_ESTRA2-OPCOES = ICON_MESSAGE_CRITICAL.
          MSG = 'Solicitação totalmente liberada'.
          FLAG_UNDO = 'N'.
          exit.
        endif.
      endloop.

      if FLAG_UNDO = 'S'.

        update ZSDT0162
          set CK_RECUSA     = 'S'
              DT_ESTORNO    = SY-DATUM
              HR_ESTORNO    = SY-UZEIT
              USER_ESTORNO  = SY-UNAME
          where BUKRS     eq WG_ESTRA-BUKRS
            and VBELN     eq WG_ESTRA-NRO_SOL_OV
            and NIVEL     eq WG_ESTRA-NIVEL
            and CK_RECUSA eq ''.

        clear: WG_INPUT_ESTRA.

*        WG_INPUT_ESTRA-BUKRS        = WG_ESTRA-BUKRS.
*        WG_INPUT_ESTRA-VBELN        = WG_ESTRA-NRO_SOL_OV.
*        WG_INPUT_ESTRA-NIVEL        = WG_ESTRA-NIVEL.
*        WG_INPUT_ESTRA-APROVADOR    = WG_ESTRA-APROVADOR.
*        WG_INPUT_ESTRA-CK_RECUSA    = 'S'.
*        WG_INPUT_ESTRA-VALOR_DE     = WG_ESTRA-VALOR_DE.
*        WG_INPUT_ESTRA-VALOR_ATE    = WG_ESTRA-VALOR_ATE.
*        WG_INPUT_ESTRA-DATA_ATUAL   = SY-DATUM.
*        WG_INPUT_ESTRA-HORA_ATUAL   = SY-UZEIT.
*        WG_INPUT_ESTRA-USUARIO      = SY-UNAME.
*        WG_INPUT_ESTRA-DT_ESTORNO   = SY-DATUM.
*        WG_INPUT_ESTRA-HR_ESTORNO   = SY-UZEIT.
*        WG_INPUT_ESTRA-USER_ESTORNO = SY-UNAME.
*
*        MODIFY ZSDT0162 FROM WG_INPUT_ESTRA.
*        COMMIT WORK.
*        CLEAR WG_INPUT_ESTRA.

      else.
        MSG = 'Devem ser reiniciadas as estratégias posteriores'.
      endif.

    elseif WG_ESTRA-OPCOES = ICON_REJECT.

      if I_MOTIVO is initial.
        call function 'CATSXT_SIMPLE_TEXT_EDITOR'
          exporting
            IM_TITLE = 'Motivo:'
          changing
            CH_TEXT  = TL_TEXTO.

        if ( TL_TEXTO[] is not initial ).
          read table TL_TEXTO[] into data(WL_TEXTO) index 1.
          WG_INPUT_ESTRA-MOTIVO = WL_TEXTO.
        endif.
      else.
        WG_INPUT_ESTRA-MOTIVO = I_MOTIVO.
      endif.

      call function 'NUMBER_GET_NEXT'
        exporting
          NR_RANGE_NR             = '01'
          OBJECT                  = 'ZSD_LOG_OV'
        importing
          NUMBER                  = VL_NUM_LOG_ID
        exceptions
          INTERVAL_NOT_FOUND      = 1
          NUMBER_RANGE_NOT_INTERN = 2
          OBJECT_NOT_FOUND        = 3
          QUANTITY_IS_0           = 4
          QUANTITY_IS_NOT_1       = 5
          INTERVAL_OVERFLOW       = 6
          BUFFER_OVERFLOW         = 7
          others                  = 8.

      WG_INPUT_ESTRA-ID_LOG       = VL_NUM_LOG_ID.
      WG_INPUT_ESTRA-BUKRS        = WG_ESTRA-BUKRS.
      WG_INPUT_ESTRA-VBELN        = WG_ESTRA-NRO_SOL_OV.
      WG_INPUT_ESTRA-NIVEL        = WG_ESTRA-NIVEL.
      WG_INPUT_ESTRA-APROVADOR    = WG_ESTRA-APROVADOR.
      WG_INPUT_ESTRA-STATUS       = 'R'.
      WG_INPUT_ESTRA-CK_RECUSA    = 'S'.
      WG_INPUT_ESTRA-VALOR_DE     = WG_ESTRA-VALOR_DE.
      WG_INPUT_ESTRA-VALOR_ATE    = WG_ESTRA-VALOR_ATE.
      WG_INPUT_ESTRA-DATA_ATUAL   = SY-DATUM.
      WG_INPUT_ESTRA-HORA_ATUAL   = SY-UZEIT.
      WG_INPUT_ESTRA-USUARIO      = SY-UNAME.
      WG_INPUT_ESTRA-DT_ESTORNO   = SY-DATUM.
      WG_INPUT_ESTRA-HR_ESTORNO   = SY-UZEIT.
      WG_INPUT_ESTRA-USER_ESTORNO = SY-UNAME.

      modify ZSDT0162 from WG_INPUT_ESTRA.
      clear WG_INPUT_ESTRA.

      update ZSDT0162
         set CK_RECUSA     = 'S'
             DT_ESTORNO    = SY-DATUM
             HR_ESTORNO    = SY-UZEIT
             USER_ESTORNO  = SY-UNAME
         where BUKRS     eq WG_ESTRA-BUKRS
           and VBELN     eq WG_ESTRA-NRO_SOL_OV
           and CK_RECUSA eq ''.


      update ZSDT0051 set STATUS = 'R' DATA_ATUAL = SY-DATUM USNAM = SY-UNAME
            where NRO_SOL_OV = WG_ESTRA-NRO_SOL_OV.
      commit work.

      MSG = 'Processamento concluído com sucesso'.
      OK = ABAP_TRUE.

    endif.

    loop at TG_ESTRA into WG_ESTRA.
      if E_ROW_ID = SY-TABIX.
        move-corresponding WG_ESTRA to W_ESTRA.
        modify T_ESTRA from W_ESTRA index SY-TABIX transporting OPCOES ESTADO.
        exit.
      endif.
    endloop.
  endloop.

endfunction.
