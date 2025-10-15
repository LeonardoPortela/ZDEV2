*&---------------------------------------------------------------------*
*&  Include           MZLES001O01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_100  OUTPUT
*&---------------------------------------------------------------------*
module STATUS_100 output.

* Verifica qual funcionalidade será habilitada
  refresh: TI_EXCL_GUICODE.

  if VG_CTRL_INOUT_1200 = CC_X.
    VG_FCODE = 'EXECOBJ'.
    append VG_FCODE to TI_EXCL_GUICODE.
    if ( STELA_1200-STATUS ne CC_A_CONFIRMAR ) or ( VG_MAIN100_TABSTRIP-ACTIVETAB ne 'TAB_CONFER' ).
      VG_FCODE = 'SEL_TODOS'.
      append VG_FCODE to TI_EXCL_GUICODE.
    endif.
    if STELA_1200-STATUS <> CC_IMPORTADO.
      VG_FCODE = 'LCCONTB'.
      append VG_FCODE to TI_EXCL_GUICODE.
    endif.
  else.
    VG_FCODE = 'LIMPAEXEC'.
    append VG_FCODE to TI_EXCL_GUICODE.
    VG_FCODE = 'LCCONTB'.
    append VG_FCODE to TI_EXCL_GUICODE.
    VG_FCODE = 'SEL_TODOS'.
    append VG_FCODE to TI_EXCL_GUICODE.
  endif.

* Seta PF-STATUS com funcionalidades da barra de botões
  set pf-status 'PFSTATUS_0100' excluding TI_EXCL_GUICODE.

* Seta o Título do programa
  set titlebar  'TITLE_0100'.

* Identifica a ABA default
  if VG_MAIN100_TABSTRIP-ACTIVETAB is initial or
    VG_DYNNR_TABSTRIP is initial.
    VG_DYNNR_TABSTRIP = '0200'.
    VG_MAIN100_TABSTRIP-ACTIVETAB = CC_100_TABSTRIP-TAB1.
  endif.

endmodule.                 " STATUS_100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_203_LOTES  OUTPUT
*&---------------------------------------------------------------------*
module CHANGE_203_LOTES output.
* Pode-se alterar a linha da tabela neste ponto...
* READ TABLE ti_203_lotes INDEX vg_col_tabcontrol-current_line.
endmodule.                 " CHANGE_203_LOTES  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_300_LACTO  OUTPUT
*&---------------------------------------------------------------------*
module CHANGE_300_LACTO output.
* Pode-se alterar a linha da tabela neste ponto...
* READ TABLE ti_300_lacto INDEX vg_col_tabcontrol-current_line.
endmodule.                 " CHANGE_300_LACTO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_400_CONFER  OUTPUT
*&---------------------------------------------------------------------*
module CHANGE_400_CONFER output.
* Pode-se alterar a linha da tabela neste ponto...
* READ TABLE ti_400_confer INDEX vg_col_tabcontrol-current_line.
endmodule.                 " CHANGE_400_CONFER  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_LOG_700  OUTPUT
*&---------------------------------------------------------------------*
module INICIALIZA_LOG_700 output.

  check: VG_INDX_SELEC_200 > 0 and VG_EXISTE_LOG_200 = CC_X.

* Lê a primeira ocorrência de detalhe para validação
  clear TI_203_LOTES.
  read table TI_203_LOTES index VG_INDX_SELEC_200.
  check: not TI_203_LOTES-ICON_LOG is initial.

* Verifica se ja existe detalhes do log exibido para o reg. selecionado
  check: SLOG_700-LOTE <> TI_203_LOTES-LOTE
     or  VG_LOG_EDIT_700 is initial.

* Dados do registro selecionado
  SLOG_700-CODTRP     = TI_203_LOTES-CODTRP.
  SLOG_700-CODPOSTO   = TI_203_LOTES-CODPOSTO.
  SLOG_700-LOTE       = TI_203_LOTES-LOTE.
  SLOG_700-CONHEC     = TI_203_LOTES-CONHEC.
  SLOG_700-DSCODTRP   = TI_203_LOTES-DSCODTRP.
  SLOG_700-DSCODPOSTO = TI_203_LOTES-DSCODPOSTO.

* Busca informações do log
  select MSGV1
    into table TI_700_LOG
    from ZLEST0008
   where LOTE = SLOG_700-LOTE
     and MSGTYP <> CC_S.

* Verifica se o controle para observações foi criado
  if not VG_LOG_EDIT_700 is bound.

    VG_LOG_MOD_READONLY_700 = CL_GUI_TEXTEDIT=>TRUE.

    if not VG_LOG_CONTAINER_700 is bound.
      create object VG_LOG_CONTAINER_700
        exporting
          CONTAINER_NAME = VG_LOG_CONT_NAME_700.
    endif.

    create object VG_LOG_EDIT_700
      exporting
        PARENT                     = VG_LOG_CONTAINER_700
        WORDWRAP_MODE              = 2
        WORDWRAP_POSITION          = 100
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.

    call method VG_LOG_EDIT_700->SET_STATUSBAR_MODE
      exporting
        STATUSBAR_MODE = 0.

  endif.

  call method VG_LOG_EDIT_700->SET_TEXT_AS_R3TABLE
    exporting
      TABLE = TI_700_LOG.

  call method VG_LOG_EDIT_700->SET_FOCUS
    exporting
      CONTROL = VG_LOG_EDIT_700.

  call method CL_GUI_CFW=>FLUSH
    exceptions
      others = 1.

  call method VG_LOG_EDIT_700->SET_READONLY_MODE
    exporting
      READONLY_MODE = VG_LOG_MOD_READONLY_700.

endmodule.                 " INICIALIZA_LOG_700  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_TABSTRIP_100  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_TABSTRIP_100 output.

* Visibilitade das Abas de detalhamento
* Só permite visibilidades se houver registro pesquisado
  loop at screen.
    case SCREEN-NAME.
      when CC_100_TABSTRIP-TAB2 or
           CC_100_TABSTRIP-TAB3 or
           CC_100_TABSTRIP-TAB4.
*       Há pesquisa com sucesso?
        if VG_CTRL_INOUT_1200 = CC_X.
          if SCREEN-NAME = CC_100_TABSTRIP-TAB3.
*           Conferência somente para status <> importação
            if STELA_1200-STATUS <> CC_IMPORTADO.
              SCREEN-INVISIBLE = '0'.
            else.
              SCREEN-INVISIBLE = '1'.
            endif.
          else.
*           Exibe demais abas
            SCREEN-INVISIBLE = '0'.
          endif.
        else.
          SCREEN-INVISIBLE = '1'.
        endif.
      when others.
    endcase.
    modify screen.
  endloop.

endmodule.                 " VISIBILIDADE_TABSTRIP_100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_SUBTOTAIS_203  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_SUBTOTAIS_203 output.

  loop at screen.
    if SCREEN-GROUP1 = 'SUB'.
      if VG_CTRL_INOUT_1200 = CC_X.
        SCREEN-INVISIBLE = '0'.
      else.
        SCREEN-INVISIBLE = '1'.
      endif.
      modify screen.
    endif.
  endloop.

endmodule.                 " VISIBILIDADE_SUBTOTAIS_203  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_DETALHES_300  OUTPUT
*&---------------------------------------------------------------------*
module INICIALIZA_DETALHES_300 output.

  check: VG_INDX_SELEC_200 > 0.

* Obtem documento selecionado
  read table TI_203_LOTES index VG_INDX_SELEC_200.
  if TI_300_LACTO[] is initial.
    clear SHEADER_300.
  endif.

* Verifica se houve nova consulta
  check: TI_203_LOTES-CODTRP   <> SHEADER_300-CODTRP
     or  TI_203_LOTES-CODPOSTO <> SHEADER_300-CODPOSTO
     or  TI_203_LOTES-LOTE     <> SHEADER_300-LOTE.

* Reseta valores
  refresh TI_300_LACTO.
  clear: STOTAIS_LANCTO_203.

* Alimenta dados do novo detalhe
  read table TI_COCKPIT_LANCTO into VG_WA_LANCTO
                           with key CODTRP   = TI_203_LOTES-CODTRP
                                    CODPOSTO = TI_203_LOTES-CODPOSTO
                                    LOTE     = TI_203_LOTES-LOTE
                                    LCTOCHVID = CC_A
  binary search.

  check: SY-SUBRC is initial.
  VG_INDEX = SY-TABIX.

* Dados de cabeçalho
  move: TI_203_LOTES-CODTRP     to SHEADER_300-CODTRP,
        TI_203_LOTES-CODPOSTO   to SHEADER_300-CODPOSTO,
        TI_203_LOTES-LOTE       to SHEADER_300-LOTE,
        TI_203_LOTES-DSCODTRP   to SHEADER_300-DSCODTRP,
        TI_203_LOTES-DSCODPOSTO to SHEADER_300-DSCODPOSTO,
        TI_203_LOTES-DATAFECHAMENTO to SHEADER_300-DATA_FECHAMENTO.

  if SHEADER_300-DATA_FECHAMENTO is initial.
    move SY-DATUM to SHEADER_300-DATA_FECHAMENTO.
  endif.

  do.
    move-corresponding VG_WA_LANCTO to TI_300_LACTO.
    clear TI_300_LACTO-MARK.
    append TI_300_LACTO.

*   Totaliza subtotais
    add TI_300_LACTO-VLRIMPORTADO
     to STOTAIS_LANCTO_203-TOT_VIMPORTADO.

    add TI_300_LACTO-VLRDIFERENCA
     to STOTAIS_LANCTO_203-TOT_VDIFERENCA.

    add TI_300_LACTO-VLRORIGEM
     to STOTAIS_LANCTO_203-TOT_VORIGEM.

    if TI_300_LACTO-CHVID ne '20'.
      add TI_300_LACTO-VLRPROGRAMADO to STOTAIS_LANCTO_203-TOT_VPROGRAMADO.
      add TI_300_LACTO-VLRCONFIRMADO to STOTAIS_LANCTO_203-TOT_VCONFIRMADO.
    else.
      subtract TI_300_LACTO-VLRPROGRAMADO from STOTAIS_LANCTO_203-TOT_VPROGRAMADO.
      subtract TI_300_LACTO-VLRCONFIRMADO from STOTAIS_LANCTO_203-TOT_VCONFIRMADO.
    endif.

    add TI_300_LACTO-PESO_ORIGEM
     to STOTAIS_LANCTO_203-TOT_PORIGEM.

    add TI_300_LACTO-PESO_IMPORTADO
     to STOTAIS_LANCTO_203-TOT_PIMPORTADO.

    add TI_300_LACTO-PESO_CONFIRMADO
     to STOTAIS_LANCTO_203-TOT_PCONFIRMADO.

*   Lê o próximo registro
    add 1 to VG_INDEX.
    read table TI_COCKPIT_LANCTO into VG_WA_LANCTO index VG_INDEX.
    if SY-SUBRC              <> 0                     or
       TI_203_LOTES-CODTRP   <> VG_WA_LANCTO-CODTRP   or
       TI_203_LOTES-CODPOSTO <> VG_WA_LANCTO-CODPOSTO or
       TI_203_LOTES-LOTE     <> VG_WA_LANCTO-LOTE.
      exit.
    endif.
  enddo.

  describe table TI_300_LACTO lines VG_SBS300_TABCONTROL-LINES.
  clear: TI_300_LACTO.

endmodule.                 " INICIALIZA_DETALHES_300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_BOTOES_202  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_BOTOES_202 output.

  if STELA_1200-STATUS = CC_IMPORTADO.
    if VG_AUT_ELIMINAR_LOTE is initial.
*     Valor Objeto eliminar lote a ser validado
      VG_AUT_ELIMINAR_LOTE = '1'.
      perform CHECK_OBJAUT_AUTHORIZACAO using VG_AUT_ELIMINAR_LOTE
                                     changing VG_TEM_AUTORIZ.
    endif.
  else.
    clear: VG_TEM_AUTORIZ,
           VG_AUT_ELIMINAR_LOTE.
  endif.

  loop at screen.
    if SCREEN-GROUP1 = 'GP2'.
      if ( VG_CTRL_INOUT_1200 = CC_X
        and STELA_1200-STATUS = CC_IMPORTADO ) or ( SCREEN-NAME eq 'ENVMAIL' ) or ( SCREEN-NAME eq 'EXP_EXCEL' ).
        SCREEN-INPUT = '1'.
      else.
        SCREEN-INPUT = '0'.
      endif.
    endif.
    if SCREEN-NAME = 'DELLOTE'.
      if VG_TEM_AUTORIZ is initial.
        SCREEN-INVISIBLE = '1'.
      else.
        SCREEN-INVISIBLE = '0'.
      endif.
    endif.
    modify screen.
  endloop.

endmodule.                 " VISIBILIDADE_BOTOES_202  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_DETALHES_400  OUTPUT
*&---------------------------------------------------------------------*
module INICIALIZA_DETALHES_400 output.

  data: LP_ADIANTAMENTO   type WRBTR,
        LP_VALOR_CONFIR   type WRBTR,
        LP_VALOR_MANUAL   type WRBTR,
        LP_VALOR_ACRDECR  type WRBTR,
        LI_INDEX          type I,
        L_DATA_CHAR       type CHAR10,
        VL_PESO_DIFERENCA type BRGEW_AP,
        VL_VLR_DIFERENCA  type WRBTR.

  check: VG_INDX_SELEC_200 > 0.

* Lê a primeira ocorrência do header
  clear VG_WA_400_CONFER.
  read table TI_203_LOTES  index VG_INDX_SELEC_200.
  read table TI_400_CONFER index 1 into VG_WA_400_CONFER.

* Verifica se houve nova consulta (Header Line - da chave principal)
  check: TI_203_LOTES-CODTRP   <> TI_400_CONFER-CODTRP
     or  TI_203_LOTES-CODPOSTO <> TI_400_CONFER-CODPOSTO
     or  TI_203_LOTES-LOTE     <> TI_400_CONFER-LOTE.

* Reseta valores
  refresh TI_400_CONFER.

  clear: TI_400_CONFER,
         SDETLH_CONFER_400,
         SCTRL_SALDO_400,
         SHEADER_300.

* Dados de cabeçalho
  move: TI_203_LOTES-CODTRP     to SHEADER_300-CODTRP,
        TI_203_LOTES-CODPOSTO   to SHEADER_300-CODPOSTO,
        TI_203_LOTES-LOTE       to SHEADER_300-LOTE,
        TI_203_LOTES-DSCODTRP   to SHEADER_300-DSCODTRP,
        TI_203_LOTES-DSCODPOSTO to SHEADER_300-DSCODPOSTO,
        TI_203_LOTES-DATAFECHAMENTO to SHEADER_300-DATA_FECHAMENTO.

  if SHEADER_300-DATA_FECHAMENTO is initial.
    move SY-DATUM to SHEADER_300-DATA_FECHAMENTO.
  endif.

* Alimenta dados do novo detalhe
  read table TI_COCKPIT_LANCTO into VG_WA_LANCTO
                           with key CODTRP   = TI_203_LOTES-CODTRP
                                    CODPOSTO = TI_203_LOTES-CODPOSTO
                                    LOTE     = TI_203_LOTES-LOTE
  binary search.

  check: SY-SUBRC is initial.
  VG_INDEX = SY-TABIX.

  clear: LP_VALOR_CONFIR,
         LP_VALOR_MANUAL.
  do.
    move-corresponding VG_WA_LANCTO to TI_400_CONFER.
    move: TI_203_LOTES-STATUS       to TI_400_CONFER-STATUS,
          TI_203_LOTES-DSCODTRP     to TI_400_CONFER-DSCODTRP,
          TI_203_LOTES-DSCODPOSTO   to TI_400_CONFER-DSCODPOSTO.

    if TI_400_CONFER-STATUS = CC_CONFIRMADO.

      read table TI_COCKPIT_CONFER into VG_WA_CONFER
        with key CODTRP   = TI_400_CONFER-CODTRP
                 CODPOSTO = TI_400_CONFER-CODPOSTO
                 LOTE     = TI_400_CONFER-LOTE
                 CHVID    = TI_400_CONFER-CHVID
      binary search.
      if SY-SUBRC is initial.
        move: VG_WA_CONFER-OBS_CONFER to TI_400_CONFER-OBSERVACOES.
      endif.

    endif.

*   Sumariza Acréscimo/Decréscimo
    if VG_WA_LANCTO-CTLGCHAVID = CC_CHVID_ADIANT.
      read table TI_COCKPIT_ACRDECR into VG_WA_ACRDECR
                                with key CODTRP = TI_203_LOTES-CODTRP
                                       CODPOSTO = TI_203_LOTES-CODPOSTO
                                           LOTE = TI_203_LOTES-LOTE
      binary search.
      if SY-SUBRC is initial.
        LI_INDEX = SY-TABIX.
        do.
          if VG_WA_ACRDECR-ACDCTIPO eq CC_3_ADACDC.
            if ( VG_WA_ACRDECR-CHVID eq '18' ) or
               ( VG_WA_ACRDECR-CHVID eq '20' ) or
               ( VG_WA_ACRDECR-CHVID eq '26' ) or
               ( VG_WA_ACRDECR-CHVID eq '28' ).
              subtract VG_WA_ACRDECR-WRBTR from TI_400_CONFER-VALOR_ACDC_AD.
            else.
              add VG_WA_ACRDECR-WRBTR to TI_400_CONFER-VALOR_ACDC_AD.
            endif.
          endif.
          add 1 to LI_INDEX.
          read table TI_COCKPIT_ACRDECR into VG_WA_ACRDECR
                                       index LI_INDEX.
          if SY-SUBRC <> 0 or
             VG_WA_ACRDECR-CODTRP   <> TI_203_LOTES-CODTRP  or
             VG_WA_ACRDECR-CODPOSTO <> TI_203_LOTES-CODPOSTO.
            exit.
          endif.
        enddo.
        add TI_400_CONFER-VALOR_ACDC_AD to LP_VALOR_ACRDECR.
      endif.
    endif.

*   Sumariza valores de conferência
    if VG_WA_LANCTO-LCTOCHVID = CC_M.
      move: CC_X                       to TI_400_CONFER-ACRESCENTADO,
            VG_WA_LANCTO-VLRCONFIRMADO to TI_400_CONFER-VALOR_ACRESC,
            VG_WA_LANCTO-DTACHEG       to TI_400_CONFER-DATA_ACRESC.
      add  VG_WA_LANCTO-VLRCONFIRMADO  to LP_VALOR_MANUAL.
    else.
      if VG_WA_LANCTO-CHVID eq '20'.
        subtract VG_WA_LANCTO-VLRCONFIRMADO from LP_VALOR_CONFIR.
      else.
        if VG_WA_LANCTO-VLRCONFIRALTER is initial.
          if  VG_WA_LANCTO-VLRCONFIRMADO is initial.
            add  VG_WA_LANCTO-VLRIMPORTADO to LP_VALOR_CONFIR.
          else.
            add  VG_WA_LANCTO-VLRCONFIRMADO to LP_VALOR_CONFIR.
          endif.
        else.
          add  VG_WA_LANCTO-VLRCONFIRALTER to LP_VALOR_CONFIR.
        endif.
      endif.
    endif.

    move: VG_WA_LANCTO-PESO_ORIGEM      to TI_400_CONFER-PESO_ORIGEM,
          VG_WA_LANCTO-PESO_IMPORTADO   to TI_400_CONFER-PESO_IMPORTADO,
          VG_WA_LANCTO-VLRIMPORTADO     to TI_400_CONFER-VLR_IMPORTADO,
          VG_WA_LANCTO-VLRPROGRAMADO    to TI_400_CONFER-VLR_PROGRAMADO,
          VG_WA_LANCTO-DOCSAP1          to TI_400_CONFER-DOCSAP1,
          VG_WA_LANCTO-DOCSAP2          to TI_400_CONFER-DOCSAP2,
          VG_WA_LANCTO-DOCSAP3          to TI_400_CONFER-DOCSAP3.

    if VG_WA_LANCTO-PESO_CONFIRALTER is initial.
      move: VG_WA_LANCTO-PESO_CONFIRMADO to TI_400_CONFER-PESO_CONFERIDO.
      VL_PESO_DIFERENCA = VG_WA_LANCTO-PESO_CONFIRMADO - VG_WA_LANCTO-PESO_ORIGEM.
      VL_VLR_DIFERENCA  = VG_WA_LANCTO-VLRCONFIRMADO - VG_WA_LANCTO-VLRPROGRAMADO.
      move: VL_PESO_DIFERENCA to TI_400_CONFER-PESO_DIFERENCA.
*---> 07/06/2023 - Migração S4 - JS
*            VL_VLR_DIFERENCA  TO TI_400_CONFER-VLR_DIFERENCA.
      TI_400_CONFER-VLR_DIFERENCA = conv #( VL_VLR_DIFERENCA ).
*<--- 07/06/2023 - Migração S4 - JS

    else.
      move: VG_WA_LANCTO-PESO_CONFIRALTER to TI_400_CONFER-PESO_CONFERIDO.
      VL_PESO_DIFERENCA = VG_WA_LANCTO-PESO_CONFIRALTER - VG_WA_LANCTO-PESO_ORIGEM.
      VL_VLR_DIFERENCA  = VG_WA_LANCTO-VLRCONFIRALTER - VG_WA_LANCTO-VLRPROGRAMADO.
      move: VL_PESO_DIFERENCA to TI_400_CONFER-PESO_DIFERENCA.
*---> 07/06/2023 - Migração S4 - JS
*            VL_VLR_DIFERENCA  TO TI_400_CONFER-VLR_DIFERENCA.
      TI_400_CONFER-VLR_DIFERENCA = conv #( VL_VLR_DIFERENCA ).
*<--- 07/06/2023 - Migração S4 - JS
    endif.

    if VG_WA_LANCTO-VLRCONFIRALTER is not initial.
      move: VG_WA_LANCTO-VLRCONFIRALTER  to TI_400_CONFER-VLR_CONFERIDO.
    else.
      move: VG_WA_LANCTO-VLRCONFIRMADO   to TI_400_CONFER-VLR_CONFERIDO.
    endif.
* Recuperando os valores delta (quebra/sobra/perda)
* Dados de calculo - Deltas
    clear: VG_WA_DELTAS.
    read table TI_COCKPIT_DELTAS into VG_WA_DELTAS
                             with key CODTRP = VG_WA_LANCTO-CODTRP
                                    CODPOSTO = VG_WA_LANCTO-CODPOSTO
                                        LOTE = VG_WA_LANCTO-LOTE
                                    CTAFRETE = VG_WA_LANCTO-CTAFRETE
                                      CONHEC = VG_WA_LANCTO-CONHEC
                                       CHVID = VG_WA_LANCTO-CHVID
                                    DATALOTE = VG_WA_LANCTO-DATALOTE
                             binary search.
    if SY-SUBRC is initial.
      move: VG_WA_DELTAS-VLRPERDA        to TI_400_CONFER-VLR_PERDA,
            VG_WA_DELTAS-DIFER_TRANSP    to TI_400_CONFER-VLR_SOBRA_QUEBRA,
            VG_WA_DELTAS-VLRIMP_RETIDOS  to TI_400_CONFER-VLR_IMP_RETIDO,
            VG_WA_DELTAS-VLRSEGURO       to TI_400_CONFER-VLR_SEGURO,
            VG_WA_DELTAS-VLRADIANTAMENTO to TI_400_CONFER-VLR_ADIANTAMENTO,
            VG_WA_DELTAS-VLRFRETE        to TI_400_CONFER-VLR_TARIFA.
    endif.

    append TI_400_CONFER.
    clear TI_400_CONFER.

*   Lê o próximo registro
    add 1 to VG_INDEX.
    read table TI_COCKPIT_LANCTO into VG_WA_LANCTO index VG_INDEX.
    if SY-SUBRC              <> 0                     or
       TI_203_LOTES-CODTRP   <> VG_WA_LANCTO-CODTRP   or
       TI_203_LOTES-CODPOSTO <> VG_WA_LANCTO-CODPOSTO or
       TI_203_LOTES-LOTE     <> VG_WA_LANCTO-LOTE.
      exit.
    endif.
  enddo.

* Obtem o valor de adiantamento

* ---> S4 Migration - 14/06/2023 - MA
*  select WRBTR
*    into LP_ADIANTAMENTO
*    from BSEG
*      up to 1 rows
*   where BUKRS = TI_203_LOTES-BUKRS
*     and BELNR = TI_203_LOTES-DOCSAPADTO
*     and GJAHR = TI_203_LOTES-GJAHR.
*  endselect.

  data: IT_BSEG type FAGL_T_BSEG.

  call function 'FAGL_GET_BSEG'
    exporting
      I_BUKRS   = TI_203_LOTES-BUKRS
      I_BELNR   = TI_203_LOTES-DOCSAPADTO
      I_GJAHR   = TI_203_LOTES-GJAHR
    importing
      ET_BSEG   = IT_BSEG
    exceptions
      NOT_FOUND = 1
      others    = 2.

  read table IT_BSEG into data(IS_BSEG) index 1.

  if SY-SUBRC = 0.
    move IS_BSEG-WRBTR to LP_ADIANTAMENTO.
  endif.
*<--- S4 Migration - 14/06/2023 - MA

* Dados para lançamento
  SCTRL_SALDO_400-SALDO_ATUAL = ( LP_ADIANTAMENTO - LP_VALOR_CONFIR )
                              + LP_VALOR_ACRDECR.
  if LP_VALOR_MANUAL > 0.
    if SCTRL_SALDO_400-SALDO_ATUAL < 0.
      add LP_VALOR_MANUAL to SCTRL_SALDO_400-SALDO_ATUAL.
    else.
      subtract LP_VALOR_MANUAL from SCTRL_SALDO_400-SALDO_ATUAL.
    endif.
  endif.
  SDETLH_CONFER_400-SALDO_HISTORICO = SCTRL_SALDO_400-SALDO_ATUAL.
  if SCTRL_SALDO_400-SALDO_ATUAL < 0.
    SCTRL_SALDO_400-HIST_NEGAT = CC_X.
  endif.
  sort TI_400_CONFER by CODTRP CODPOSTO LOTE CONHEC.
* Atualiza valores tabcontrol
  describe table TI_400_CONFER lines VG_SBS400_TABCONTROL-LINES.
  clear TI_400_CONFER.

* Verifica se o controle para observações foi criado
  check: not VG_OBS_EDIT_400 is bound.

  VG_OBS_MOD_READONLY_400 = CL_GUI_TEXTEDIT=>TRUE.

  if not VG_OBS_CONTAINER_400 is bound.
    create object VG_OBS_CONTAINER_400
      exporting
        CONTAINER_NAME = VG_OBS_CONT_NAME_400.
  endif.

  create object VG_OBS_EDIT_400
    exporting
      PARENT                     = VG_OBS_CONTAINER_400
      WORDWRAP_MODE              = 2
      WORDWRAP_POSITION          = 100
      WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.

  call method VG_OBS_EDIT_400->SET_STATUSBAR_MODE
    exporting
      STATUSBAR_MODE = 0.

  call method VG_OBS_EDIT_400->SET_TOOLBAR_MODE
    exporting
      TOOLBAR_MODE = 0.

  call method VG_OBS_EDIT_400->SET_READONLY_MODE
    exporting
      READONLY_MODE = VG_OBS_MOD_READONLY_400.

  call method CL_GUI_CFW=>FLUSH
    exceptions
      others = 1.

endmodule.                 " INICIALIZA_DETALHES_400  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_402  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_402 output.

* Barra de botões e títulos
  set pf-status 'PFSTATUS_0402'.
  set titlebar 'TITLE_0402'.

* Valores iniciais
  if SNLANCTO_CONFER_400-DATA is initial.
    SNLANCTO_CONFER_400-DATA = SY-DATUM.
  endif.

endmodule.                 " STATUS_402  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_BOTOES_401  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_BOTOES_401 output.

* Desabilita botões de lançamento quando:
* 1) Saldo é zero e não existe lançamento manual
* 2) O status do documento estiver confirmado (consulta)
  clear VG_INDEX.

* Verifica se existe saldo manual
  if TI_203_LOTES-STATUS <> CC_CONFIRMADO.
    read table TI_400_CONFER with key ACRESCENTADO = CC_X
                             transporting no fields.
    if SY-SUBRC is initial.
      VG_INDEX = SY-TABIX.
    endif.
  endif.

* Desabilita botões para lançamento manual
  loop at screen.
    SCREEN-INPUT = '1'.
    if TI_203_LOTES-STATUS = CC_CONFIRMADO and
       SCREEN-NAME = 'LCCONTB_401'.
      SCREEN-INPUT = '0'.
    elseif SCREEN-GROUP1(2) = 'SL'.
      if SCREEN-GROUP1 = 'SLC' and
         SCTRL_SALDO_400-SALDO_ATUAL is initial.
        SCREEN-INPUT = '0'.
      elseif SCREEN-GROUP1 = 'SLM' and VG_INDEX is initial.
        SCREEN-INPUT = '0'.
      endif.
    endif.
    modify screen.
  endloop.

endmodule.                 " VISIBILIDADE_BOTOES_401  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_EDICAO_402  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_EDICAO_402 output.

  check: SCTRL_SALDO_400-EDIT_LANC = CC_X.

* Desabilita campos chaves na edição de lançamento manual
  loop at screen.
    check: SCREEN-GROUP1 = 'EDT'.
    SCREEN-INPUT = '0'.
    modify screen.
  endloop.

endmodule.                 " VISIBILIDADE_EDICAO_402  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_VALORES_400  OUTPUT
*&---------------------------------------------------------------------*
module VISIBILIDADE_VALORES_400 output.

* 1) Desabilita campos chaves na edição de lançamento manual
* 2) Habilita ou Desabilita alteração de valor conferido
  loop at screen.

    check: SCREEN-GROUP1 = 'DOC' or SCREEN-GROUP1 = 'PES' or
           SCREEN-GROUP1 = 'AD1' or SCREEN-GROUP1 = 'RE1'
           or SCREEN-GROUP1 = 'RE2' or SCREEN-GROUP1 = 'RE3'
           or SCREEN-GROUP1 = 'RE4' or SCREEN-GROUP1 = 'MSG'
           or SCREEN-GROUP1 = 'DTF'.

    if SCREEN-GROUP1 = 'AD1'.
      if SDETLH_CONFER_400-VLR_ACRE_DECRE is initial.
*       Disponibiliza valor de acréscimo e/ou decréscimo
        SCREEN-INVISIBLE = '1'.
      else.
*       Não disponibiliza valor de acréscimo e/ou decréscimo
        SCREEN-INVISIBLE = '0'.
      endif.
    else.
      if TI_203_LOTES-STATUS = CC_CONFIRMADO.
        if SCREEN-GROUP1 = 'PES'.
*         Não disponibiliza botão para alteração de peso conferido
          SCREEN-INVISIBLE = '1'.
        else.
*         Disponibiliza documentos SAP para status confirmado
          SCREEN-INVISIBLE = '0'.
        endif.
      endif.
      if SCREEN-GROUP1 = 'PES'.
        if ( ( SDETLH_CONFER_400-PESO_CONFERIDO is initial ) and ( SDETLH_CONFER_400-CHVID ne '2' ) and ( SDETLH_CONFER_400-CHVID ne '27' ) ) or
           SDETLH_CONFER_400-LCTOCHVID = CC_M.
*           Não disponibiliza botão para alteração de peso conferido
          SCREEN-INVISIBLE = '1'.
        else.
*           Permite alteração de valor conferido
          SCREEN-INVISIBLE = '0'.
        endif.
      else.
*         Não disponibiliza docto SAP para status direrente confirmado
        SCREEN-INVISIBLE = '1'.
      endif.

      if SCREEN-GROUP1 = 'RE1'.
        if ( ( SDETLH_CONFER_400-DOCSAP1 is initial ) and ( SDETLH_CONFER_400-DOC1MSG is not initial ) ) or
           ( ( SDETLH_CONFER_400-DOCSAP1 is initial ) and ( TI_203_LOTES-STATUS = CC_CONFIRMADO ) and ( SDETLH_CONFER_400-VLR_CONFERIDO ne 0 ) ).
          SCREEN-INVISIBLE = '0'.
        else.
          SCREEN-INVISIBLE = '1'.
        endif.
      endif.
      if SCREEN-GROUP1 = 'RE2'.
        if ( ( SDETLH_CONFER_400-DOCSAP2 is initial ) and ( SDETLH_CONFER_400-DOC2MSG is not initial ) ) or
           ( ( SDETLH_CONFER_400-DOCSAP2 is initial ) and ( TI_203_LOTES-STATUS = CC_CONFIRMADO ) and ( SDETLH_CONFER_400-VLR_PERDA ne 0 ) ).
          SCREEN-INVISIBLE = '0'.
        else.
          SCREEN-INVISIBLE = '1'.
        endif.
      endif.
      if SCREEN-GROUP1 = 'RE3'.
        if ( ( SDETLH_CONFER_400-DOCSAP3 is initial ) and ( SDETLH_CONFER_400-DOC3MSG is not initial ) ) or
           ( ( SDETLH_CONFER_400-DOCSAP3 is initial ) and ( TI_203_LOTES-STATUS = CC_CONFIRMADO ) and ( SDETLH_CONFER_400-VLR_SOBRA_QUEBRA ne 0 ) ).
          SCREEN-INVISIBLE = '0'.
        else.
          SCREEN-INVISIBLE = '1'.
        endif.
      endif.

      if SCREEN-GROUP1 = 'MSG' or SCREEN-GROUP1 = 'DOC'.
        if TI_203_LOTES-STATUS = CC_CONFIRMADO.
          SCREEN-INVISIBLE = '0'.
        else.
          SCREEN-INVISIBLE = '1'.
        endif.
      endif.
      if SCREEN-GROUP1 = 'DTF'.
        if TI_203_LOTES-STATUS = CC_CONFIRMADO.
          SCREEN-INVISIBLE = '0'.
          SCREEN-INPUT = '0'.
        else.
          SCREEN-INVISIBLE = '0'.
          SCREEN-INPUT = '1'.
        endif.
      endif.

    endif.
    modify screen.
  endloop.

endmodule.                 " VISIBILIDADE_VALORES_400  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_DETALHES_403  OUTPUT
*&---------------------------------------------------------------------*
module INICIALIZA_DETALHES_403 output.

  set titlebar  'TITLE_0403'.
  set pf-status 'PFSTATUS_0403'.

endmodule.                 " INICIALIZA_DETALHES_403  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_DETALHES_204  OUTPUT
*&---------------------------------------------------------------------*
module INICIALIZA_DETALHES_204 output.

  set titlebar  'TITLE_0204'.
  set pf-status 'PFSTATUS_0204'.

  check: TI_204H_ACRDECR[] is initial.
  clear VG_CAMPO_CURR.

* Lê documentos para análise de Acréscimo/Decréscimo
  loop at TI_IDXACRDECR.

    read table TI_203_LOTES index TI_IDXACRDECR-INDEX.

*   Histórico de lançamentos de acréscimo/decréscimo
    read table TI_COCKPIT_ACRDECR into VG_WA_ACRDECR with key CODTRP = TI_203_LOTES-CODTRP CODPOSTO = TI_203_LOTES-CODPOSTO
    binary search.

    if not ( SY-SUBRC is initial and VG_WA_ACRDECR-ACDCTIPO = CC_1_ADACDC ).
      continue.
    endif.

    VG_INDEX = SY-TABIX.
    clear VG_CAMPO_CURR.

    if not TI_204D_ACRDECR[] is initial.
*     Quebra de Linha
      clear TI_204D_ACRDECR.
      append TI_204D_ACRDECR.
    endif.

    do.
      append VG_WA_ACRDECR to TI_204D_ACRDECR.
      if ( VG_WA_ACRDECR-CHVID eq '20' ).
*       Deduzir o valor pago a maior
        multiply VG_WA_ACRDECR-WRBTR by -1.
      endif.
*     Sumariza Acréscimos
      add VG_WA_ACRDECR-WRBTR to VG_CAMPO_CURR.

      add 1 to VG_INDEX.
      read table TI_COCKPIT_ACRDECR into VG_WA_ACRDECR index VG_INDEX.
      if SY-SUBRC <> 0 or
         VG_WA_ACRDECR-CODTRP   <> TI_203_LOTES-CODTRP or
         VG_WA_ACRDECR-CODPOSTO <> TI_203_LOTES-CODPOSTO.
        exit.
      endif.
    enddo.

*   Valores para Adiantamentos...
    move: SPACE                     to TI_204H_ACRDECR-ACAO,
          TI_203_LOTES-CODTRP       to TI_204H_ACRDECR-CODTRP,
          TI_203_LOTES-DSCODTRP     to TI_204H_ACRDECR-DSCODTRP,
          TI_203_LOTES-CODPOSTO     to TI_204H_ACRDECR-CODPOSTO,
          TI_203_LOTES-DSCODPOSTO   to TI_204H_ACRDECR-DSCODPOSTO,
          TI_203_LOTES-LOTE         to TI_204H_ACRDECR-LOTE,
          TI_IDXACRDECR-VALOR       to TI_204H_ACRDECR-VLRREALIZADO,
          TI_IDXACRDECR-VALOR       to TI_204H_ACRDECR-VLRLANCTO.
    " +                                vg_campo_curr.
    append TI_204H_ACRDECR.

  endloop.

* Atualiza linhas na table control
  describe table: TI_204H_ACRDECR lines VG_SBS204H_TABCONTROL-LINES,
                  TI_204D_ACRDECR lines VG_SBS204D_TABCONTROL-LINES.

endmodule.                 " INICIALIZA_DETALHES_204  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_TABLE_CONTROL_204H  OUTPUT
*&---------------------------------------------------------------------*
module CHANGE_TABLE_CONTROL_204H output.
* Pode-se alterar a linha da tabela neste ponto...
* READ TABLE ti_204h_acrdecr INDEX vg_col_tabcontrol-current_line.
endmodule.                 " CHANGE_TABLE_CONTROL_204H  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHANGE_TABLE_CONTROL_204D  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CHANGE_TABLE_CONTROL_204D output.
*  READ TABLE ti_204h_acrdecr WITH KEY lote = ti_204d_acrdecr-lote_aplicar acao = cc_1_adacdc.
*  IF NOT sy-subrc IS INITIAL.
*    CLEAR ti_204d_acrdecr-lote_aplicar.
*    MODIFY ti_204d_acrdecr TRANSPORTING lote_aplicar.
*  ENDIF.
endmodule.                 " CHANGE_TABLE_CONTROL_204D  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_BOTOES_404  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module VISIBILIDADE_BOTOES_404 output.

* Desabilita botões para lançamento manual
  loop at screen.
    if SCREEN-NAME eq 'BTN_ANULAR'.
      if ( ( SDETLH_CONFER_400-CHVID = '1' ) or ( SDETLH_CONFER_400-CHVID = '2' ) )  and
         ( TI_203_LOTES-STATUS eq CC_A_CONFIRMAR ).
        SCREEN-INPUT = '1'.
      else.
        if ( SDETLH_CONFER_400-CHVID = '26' ) or ( SDETLH_CONFER_400-CHVID = '28' ).
          SCREEN-INPUT = '1'.
        else.
          SCREEN-INPUT = '0'.
        endif.
      endif.
      modify screen.
    endif.
  endloop.

endmodule.                 " VISIBILIDADE_BOTOES_404  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_404  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module CHECK_OKCODE_404 input.

endmodule.                 " CHECK_OKCODE_404  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0205  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0205 output.

  clear: IT_FCODE.

  if VG_CONFIRMADO_0205 is not initial.
    WA_FCODE = 'CONFIRMAR'.
    append WA_FCODE to IT_FCODE.
    WA_FCODE = 'CANCELAR'.
    append WA_FCODE to IT_FCODE.
  else.
    WA_FCODE = 'SAIR'.
    append WA_FCODE to IT_FCODE.
  endif.

  select single * into WA_ZLEST0025
    from ZLEST0025
   where CHVID eq ZLEST0022-CHVID.

  set pf-status 'PFSTATUS_0404' excluding IT_FCODE.
  set titlebar 'TITLE_0404'.

endmodule.                 " STATUS_0205  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0205  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0205 input.

  case OK_CODE_205.
    when 'CONFIRMAR'.
      modify ZLEST0022.
      VG_CONFIRMADO_0205 = 'X'.
  endcase.

endmodule.                 " USER_COMMAND_0205  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_ANULACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module EXIT_ANULACAO input.

  case OK_CODE_205.
    when 'CANCELAR'.
      leave to screen 0.
    when 'SAIR'.
      leave to screen 0.
  endcase.

endmodule.                 " EXIT_ANULACAO  INPUT

*&---------------------------------------------------------------------*
*&      Form  GERA_ANULACAO_404
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GERA_ANULACAO_404 .

  data: WA_ZLEST0022  type ZLEST0022,
        WA_SHTYP      type SHTYP,
        VG_CHAVE      type ZCHVID,
        WA_400_CONFER type TY_400_TC_CONFER.

  read table TI_400_CONFER into WA_400_CONFER with key MARK = CC_X.

  if SY-SUBRC is not initial.
    message E000 with 'Selecionar registro de históricos com chaves 01 02 26 28' .
  endif.

  if WA_400_CONFER-CHVID ne '26' and WA_400_CONFER-CHVID ne '28' and WA_400_CONFER-CHVID ne '2' and WA_400_CONFER-CHVID ne '1'.
    message E000 with 'Somente registro de históricos com chaves 01 02 26 28' .
  endif.

*   Busca o tipo do documento de transporte
  select SHTYP
      into WA_SHTYP
      from VTTK
        up to 1 rows
     where TDLNR = WA_400_CONFER-CODTRP
       and EXTI1 = WA_400_CONFER-CONHEC
       and EXTI2 = WA_400_CONFER-CTAFRETE.
  endselect.

  case WA_400_CONFER-CHVID.
    when '2'.
      VG_CHAVE = '26'.
    when '26'.
      VG_CHAVE = '27'.
    when '1'.
      VG_CHAVE = '28'.
    when '27'.
      VG_CHAVE = '29'.
  endcase.

  select single * into WA_ZLEST0022
    from ZLEST0022
   where TRANSPORTADOR eq WA_400_CONFER-CODTRP
     and POSTO         eq WA_400_CONFER-CODPOSTO
     and LOTE          eq WA_400_CONFER-LOTE
     and CHVID         eq VG_CHAVE
     and TIPTRANSP     eq WA_SHTYP
     and CTLGLANCTO    eq 'VC'
     and CONHECIMENTO  eq WA_400_CONFER-CONHEC
     and CTAFRETE      eq WA_400_CONFER-CTAFRETE.

  if SY-SUBRC is initial.
    message E000 with 'Registro com chave' VG_CHAVE ' já existe para este lote!'.
  else.
    clear: ZLEST0022.
    ZLEST0022-TRANSPORTADOR = WA_400_CONFER-CODTRP.
    ZLEST0022-POSTO         = WA_400_CONFER-CODPOSTO.
    ZLEST0022-LOTE          = WA_400_CONFER-LOTE.
    ZLEST0022-BUKRS         = TI_203_LOTES-BUKRS.
    ZLEST0022-CHVID         = VG_CHAVE.
    ZLEST0022-TIPTRANSP     = WA_SHTYP.
    ZLEST0022-CTLGLANCTO    = 'VC'.
    ZLEST0022-CONHECIMENTO  = WA_400_CONFER-CONHEC.
    ZLEST0022-CTAFRETE      = WA_400_CONFER-CTAFRETE.
    ZLEST0022-ACDCTIPO      = '1'.
    ZLEST0022-ERDAT         = SY-DATUM.
    ZLEST0022-UNAME         = SY-UNAME.
    clear: VG_CONFIRMADO_0205.
    "... STARTING AT col1 lin1 [ENDING AT col2 lin2]
    call screen 0205 starting at 07 05 ending at 130 05.
  endif.

endform.                    " GERA_ANULACAO_404

*&---------------------------------------------------------------------*
*&      Form  LISTAR_ANULACAO_404
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form LISTAR_ANULACAO_404 .

  data: IT_AUX_0025  type table of ZLEST0025 with header line,
        IT_AUX_0022  type table of TY_ZLEST0022 with header line,
        VG_AUX_TABIX type SY-TABIX,
        WA_AUX_0025  type ZLEST0025.

  clear: IT_ZLEST0022[], IT_AUX_0022[].

  select * into corresponding fields of table IT_ZLEST0022
    from ZLEST0022
   where TRANSPORTADOR eq TI_203_LOTES-CODTRP
     and POSTO         eq TI_203_LOTES-CODPOSTO
     and LOTE          eq TI_203_LOTES-LOTE
     and CHVID         in (CC_26,CC_27,CC_28,CC_29).

  if SY-SUBRC is not initial.
    message E000 with 'Não existe lançamentos de anulação para este lote!' .
  endif.

  move IT_ZLEST0022[] to IT_AUX_0022[].
  sort IT_AUX_0022 by CHVID.
  delete adjacent duplicates from IT_AUX_0022 comparing CHVID.

  select * into table IT_AUX_0025
    from ZLEST0025
     for all entries in IT_AUX_0022
   where CHVID eq IT_AUX_0022-CHVID.

  loop at IT_ZLEST0022 into WA_ZLEST0022.
    VG_AUX_TABIX = SY-TABIX.

    read table IT_AUX_0025 into WA_AUX_0025 with key CHVID = WA_ZLEST0022-CHVID.
    if SY-SUBRC is initial.
      WA_ZLEST0022-DESCHVID = WA_AUX_0025-DESCHVID.
      modify IT_ZLEST0022 from WA_ZLEST0022 index VG_AUX_TABIX transporting DESCHVID.
    endif.
  endloop.

  call screen 0206 starting at 07 05 ending at 100 20.

endform.                    " LISTAR_ANULACAO_404

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_INFO_LISTA  OUTPUT
*&---------------------------------------------------------------------*
module CTE_CRIA_INFO_LISTA output.

  constants: TABELA type STRING value 'IT_ZLEST0022'.

  if PRIM_INFO_LISTA is initial.

*   Create object for container
    create object CONTAINER_INF_LISTA
      exporting
        CONTAINER_NAME = 'ALV_LISTA_ANULA'.

    create object ALV_INF_LISTA
      exporting
        I_PARENT = CONTAINER_INF_LISTA.

    perform Z_ESTRUTURA_FIELDCAT tables IT_CATALOG_INF_LISTA using:
        TABELA 'CHVID'          text-A00 ' ' 01 002 SPACE SPACE,
        TABELA 'DESCHVID'       text-A01 ' ' 02 040 SPACE SPACE,
        TABELA 'TIPTRANSP'      text-A02 ' ' 03 003 SPACE SPACE,
        TABELA 'CONHECIMENTO'   text-A03 ' ' 04 010 SPACE SPACE,
        TABELA 'CTAFRETE'       text-A04 ' ' 05 010 SPACE SPACE,
        TABELA 'LOTE_APLICADO'  text-A06 ' ' 06 010 SPACE SPACE,
        TABELA 'OBS_CONFER'     text-A05 ' ' 07 100 SPACE SPACE.

    GS_LAYOUT-ZEBRA    = CC_X.
    GS_LAYOUT-SEL_MODE = SPACE.

    call method ALV_INF_LISTA->SET_TABLE_FOR_FIRST_DISPLAY
      exporting
        I_DEFAULT       = SPACE
        IS_LAYOUT       = GS_LAYOUT
      changing
        IT_FIELDCATALOG = IT_CATALOG_INF_LISTA
        IT_OUTTAB       = IT_ZLEST0022[].

    PRIM_INFO_LISTA = CC_X.

  endif.

  call method ALV_INF_LISTA->REFRESH_TABLE_DISPLAY.

endmodule.                 " CTE_CRIA_INFO_LISTA  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form Z_ESTRUTURA_FIELDCAT  tables IT_CATALOGO type LVC_T_FCAT
                           using P_TAB_NAME
                                 P_FIELDNAME
                                 P_TEXTO_GRANDE
                                 P_HOT
                                 P_POSICAO
                                 P_OUTPUTLEN
                                 P_FIX_COLUMN
                                 P_CONVEXIT.
  clear WA_CATALOG.
  WA_CATALOG-TABNAME     = P_TAB_NAME.
  WA_CATALOG-FIELDNAME   = P_FIELDNAME.
  WA_CATALOG-SCRTEXT_L   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_M   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_S   = P_TEXTO_GRANDE.
  WA_CATALOG-HOTSPOT     = P_HOT.
  WA_CATALOG-COL_POS     = P_POSICAO.
  WA_CATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  WA_CATALOG-FIX_COLUMN  = P_FIX_COLUMN.
  WA_CATALOG-CONVEXIT    = P_CONVEXIT.
  append WA_CATALOG to IT_CATALOGO.
endform.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0206  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0206 output.
  set pf-status 'PFSTATUS_0205'.
  set titlebar 'TITLE_0205'.
endmodule.                 " STATUS_0206  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_ANULACAO_LISTA  INPUT
*&---------------------------------------------------------------------*
module EXIT_ANULACAO_LISTA input.

  case OK_CODE_206.
    when 'SAIR'.
      leave to screen 0.
  endcase.

endmodule.                 " EXIT_ANULACAO_LISTA  INPUT
