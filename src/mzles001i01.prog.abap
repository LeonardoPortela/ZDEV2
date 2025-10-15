*&---------------------------------------------------------------------*
*&  Include           MZLES001I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  EXIT_APLICATIVO  INPUT
*&---------------------------------------------------------------------*
module EXIT_APLICATIVO input.

  perform LIBERA_LOTES_USUARIO.

  leave program.

endmodule.                 " EXIT_APLICATIVO  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_203  INPUT
*&---------------------------------------------------------------------*
module READ_TABLE_CONTROL_203 input.

  data: WA_203_LOTES type TY_203_TC_LOTES,
        WA_203_LOTE2 type TY_203_TC_LOTES.

  data: begin of WA_ENQ.
          include structure SEQG7.
  data: end of WA_ENQ.

  data: GARG like SEQG3-GARG,
        ENQ  like standard table of WA_ENQ.

  read table TI_203_LOTES into WA_203_LOTE2 index VG_SBS203_TABCONTROL-CURRENT_LINE.

* Atualiza flag de seleção
  modify TI_203_LOTES index VG_SBS203_TABCONTROL-CURRENT_LINE transporting MARK.

  read table TI_203_LOTES into WA_203_LOTES index VG_SBS203_TABCONTROL-CURRENT_LINE.

  check not ( ( WA_203_LOTE2-MARK eq CC_X ) and ( WA_203_LOTES-MARK eq CC_X ) ).

  check not TI_203_LOTES[] is initial.

  if WA_203_LOTES-MARK eq CC_X.

    call function 'ZENQUEUE_ZLOTE_POSTOS'
      exporting
        TRANSPORTADOR  = WA_203_LOTES-CODTRP
        POSTO          = WA_203_LOTES-CODPOSTO
        LOTE           = WA_203_LOTES-LOTE
      exceptions
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        others         = 3.

    case SY-SUBRC.
      when 1.

        concatenate SY-MANDT WA_203_LOTES-CODTRP WA_203_LOTES-CODPOSTO WA_203_LOTES-LOTE into GARG.

        call function 'ENQUE_READ2'
          exporting
            GNAME  = 'ZLEST0015'
            GARG   = GARG
            GUNAME = '*'
          tables
            ENQ    = ENQ.

        read table ENQ into WA_ENQ with key GNAME = 'ZLEST0015'.

        clear: WA_203_LOTES-MARK.

        modify TI_203_LOTES index VG_SBS203_TABCONTROL-CURRENT_LINE from WA_203_LOTES transporting MARK.

        message S000 with 'Lote' WA_203_LOTES-LOTE 'bloqueado por usuário' WA_ENQ-GUNAME.

      when 2.

        message I000 with 'Erro em bloqueio!'.

    endcase.

  else.
    call function 'ZDEQUEUE_ZLOTE_POSTOS'
      exporting
        TRANSPORTADOR = WA_203_LOTES-CODTRP
        POSTO         = WA_203_LOTES-CODPOSTO
        LOTE          = WA_203_LOTES-LOTE.
  endif.


endmodule.                 " READ_TABLE_CONTROL_203  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
module USER_COMMAND_100 input.

* Salva o OK_CODE para ser validado nas telas subsequentes (Abas).
  VG_SAVE_OK = OK_CODE.
  clear OK_CODE.

  case VG_SAVE_OK.
    when 'VOLTAR'.
      if VG_DYNNR_TABSTRIP = '0200'.
        perform LIBERA_LOTES_USUARIO.
        leave program.
      else.
        VG_DYNNR_TABSTRIP = '0200'.
        VG_MAIN100_TABSTRIP-ACTIVETAB = CC_100_TABSTRIP-TAB1.
      endif.
    when 'SEL_TODOS'.
      perform SELECIONAR_TODOS_CONFERENCIA.
    when 'LIMPAEXEC'.
      perform RENOVA_TELA_PARA_PESQUISA.
      perform RESETA_VLRSALVO_SELECAO_1200.
      clear VG_SAVE_OK.
    when others.
*--------------
* Nota: OK-CODE
*--------------
*     1) Não controla navegação de ABA quando o controle ativo estiver
*        na tela de lote (200/203), devido obrigatoriedade de seleção.
*     2) Demais funcionalidades na barra de botões estão sendo delegadas
*        nas telas responsáveis pela informações...
*         Exemplo: 'EXECOBJ'  -> tela 1200 (MZLES001P200)
*                  'LCCONTB'  -> tela 200/203
      if VG_MAIN100_TABSTRIP-ACTIVETAB <> CC_100_TABSTRIP-TAB1.
        perform CHECK_COMMAND_TABSTRIP.
      endif.

  endcase.

endmodule.                 " USER_COMMAND_100  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_300  INPUT
*&---------------------------------------------------------------------*
module READ_TABLE_CONTROL_300 input.
*
** Atualiza flag de seleção
*  MODIFY ti_300_lacto INDEX vg_sbs300_tabcontrol-current_line
*                     TRANSPORTING mark.

endmodule.                 " READ_TABLE_CONTROL_300  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_400  INPUT
*&---------------------------------------------------------------------*
module READ_TABLE_CONTROL_400 input.

* Atualiza flag de seleção
  modify TI_400_CONFER index VG_SBS400_TABCONTROL-CURRENT_LINE
                       transporting MARK CHECK.

endmodule.                 " READ_TABLE_CONTROL_400  INPUT


*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_200  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_203 input.

* Verifica controle de navegação de ABAS
* Nota: O controle se faz aqui devido a obrigatoriedade de seleção
* de um registro para o seu detalhamento
  perform CHECK_COMMAND_TABSTRIP.

* Determina função a ser executada
  case VG_SAVE_OK.
    when 'APROV_203'.
      clear VG_SAVE_OK.
      perform ALTERA_STATBL_LIBERADO_203.
    when 'DAPROV_203'.
      clear VG_SAVE_OK.
      perform ALTERA_STATBL_BLOQUEADO_203.
    when 'LCCONTB'.
      clear VG_SAVE_OK.
      perform GERA_LANCTO_CONTABIL_203.
    when 'MARK_ALL'.
      clear VG_SAVE_OK.
      TI_203_LOTES-MARK = CC_X.
      modify TI_203_LOTES transporting MARK
                          where MARK le SPACE.
    when 'DMARK_ALL'.
      clear VG_SAVE_OK.
      TI_203_LOTES-MARK = SPACE.
      modify TI_203_LOTES transporting MARK
                          where MARK gt SPACE.
    when 'DELLOTE'.
      clear VG_SAVE_OK.
      perform EXCLUI_LOTE_STATUS_I_203.
    when 'ENVMAIL'.
      clear VG_SAVE_OK.
      perform ENVIAR_EMAIL.
    when 'EXP_EXCEL'  .
      clear VG_SAVE_OK.
      perform GERA_EXCEL.
  endcase.

* Limpa controle de funcionalidade processada
  clear: VG_SAVE_OK.

endmodule.                 " CHECK_OKCODE_203  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_400  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_400 input.

  data: VL_OBJ_KEY        type ZLES_COCKPIT_CONFER-OBJ_KEY,
        LC_CTLG_TIPTRP(7) type C,
        LD_DATA_LANCTO    type D,
        LF_HA_DOC_OK.

  data: LC_SHTYP      type SHTYP,
        VG_WA_REENVIO type ZLES_COCKPIT_CONFER.

  select SHTYP
    into LC_SHTYP
    from VTTK
      up to 1 rows
   where TDLNR = TI_400_CONFER-CODTRP
     and EXTI1 = TI_400_CONFER-CONHEC
     and EXTI2 = TI_400_CONFER-CTAFRETE.
  endselect.

  if VG_SAVE_OK = 'EDTPCONF'.

    perform NOVO_CALCULO_PESO_CONFIRMADO.
    clear: VG_SAVE_OK.

  elseif VG_SAVE_OK = 'REVDOCSAP1' or VG_SAVE_OK = 'REVDOCSAP2' or VG_SAVE_OK = 'REVDOCSAP3' or VG_SAVE_OK = 'REVDOCSAP4'.

* Lê o primeiro documento marcado para detalhamento
    read table TI_400_CONFER with key MARK = CC_X.
    check: SY-SUBRC is initial.

    VG_INDEX = SY-TABIX.

* Garante que o registro do Header é o mesmo da conferência
    check: TI_400_CONFER-CODTRP   = TI_203_LOTES-CODTRP
      and  TI_400_CONFER-CODPOSTO = TI_203_LOTES-CODPOSTO
      and  TI_400_CONFER-LOTE     = TI_203_LOTES-LOTE.

    read table TI_COCKPIT_CONFER into VG_WA_CONFER
                             with key CODTRP       = TI_400_CONFER-CODTRP
                                      CODPOSTO     = TI_400_CONFER-CODPOSTO
                                      LOTE         = TI_400_CONFER-LOTE
                                      CONHECIMENTO = TI_400_CONFER-CONHEC
                                      CHVID        = TI_400_CONFER-CHVID
    binary search.
    if SY-SUBRC is initial.
      VG_INDEX = SY-TABIX.
      "CTLGLANCTO
      do 4 times.
        VL_OBJ_KEY = VG_WA_CONFER-OBJ_KEY.
        if VG_WA_CONFER-ERR_MSG is not initial and VG_WA_CONFER-CTLGLANCTO = 'VC' and VG_SAVE_OK = 'REVDOCSAP1'.
          perform REENVIO_DOC_SAP using VL_OBJ_KEY VG_INDEX.
          exit.
        endif.
        if VG_SAVE_OK = 'REVDOCSAP2'.

          read table TI_COCKPIT_CONFER into VG_WA_REENVIO
                                   with key CODTRP       = TI_400_CONFER-CODTRP
                                            CODPOSTO     = TI_400_CONFER-CODPOSTO
                                            LOTE         = TI_400_CONFER-LOTE
                                            CONHECIMENTO = TI_400_CONFER-CONHEC
                                            CHVID        = TI_400_CONFER-CHVID
                                            CTLGLANCTO   = 'Q'.
          if SY-SUBRC is initial.
            if VG_WA_REENVIO-ERR_MSG is not initial and ( VG_WA_REENVIO-CTLGLANCTO = 'Q' or VG_WA_REENVIO-CTLGLANCTO = 'S' ).
              VL_OBJ_KEY = VG_WA_REENVIO-OBJ_KEY.
              perform REENVIO_DOC_SAP using VL_OBJ_KEY SY-TABIX.
              exit.
            endif.
          endif.

          read table TI_COCKPIT_CONFER into VG_WA_REENVIO
                                   with key CODTRP       = TI_400_CONFER-CODTRP
                                            CODPOSTO     = TI_400_CONFER-CODPOSTO
                                            LOTE         = TI_400_CONFER-LOTE
                                            CONHECIMENTO = TI_400_CONFER-CONHEC
                                            CHVID        = TI_400_CONFER-CHVID
                                            CTLGLANCTO   = 'S'.
          if SY-SUBRC is initial.
            if VG_WA_REENVIO-ERR_MSG is not initial and ( VG_WA_REENVIO-CTLGLANCTO = 'Q' or VG_WA_REENVIO-CTLGLANCTO = 'S' ).
              VL_OBJ_KEY = VG_WA_REENVIO-OBJ_KEY.
              perform REENVIO_DOC_SAP using VL_OBJ_KEY SY-TABIX.
              exit.
            endif.
          endif.

        endif.
        if VG_SAVE_OK = 'REVDOCSAP3'.
          read table TI_COCKPIT_CONFER into VG_WA_REENVIO
                                   with key CODTRP       = TI_400_CONFER-CODTRP
                                            CODPOSTO     = TI_400_CONFER-CODPOSTO
                                            LOTE         = TI_400_CONFER-LOTE
                                            CONHECIMENTO = TI_400_CONFER-CONHEC
                                            CHVID        = TI_400_CONFER-CHVID
                                            CTLGLANCTO   = 'P'.
          if SY-SUBRC is initial.
            if VG_WA_REENVIO-ERR_MSG is not initial and VG_WA_REENVIO-CTLGLANCTO = 'P'.
              VL_OBJ_KEY = VG_WA_REENVIO-OBJ_KEY.
              perform REENVIO_DOC_SAP using VL_OBJ_KEY SY-TABIX.
              exit.
            endif.
          endif.
        endif.
*         if vg_wa_confer-err_msg is not initial and vg_wa_confer-CTLGLANCTO = 'P' and vg_save_ok = 'REVDOCSAP4'.
*            perform reenvio_doc_sap USING USING vl_obj_key
*                                          vg_index.
*            exit.
*         endif.
        if TI_203_LOTES-STATUS eq 'C'.

          if TI_400_CONFER-DATA_ACRESC is initial.
            LD_DATA_LANCTO = SY-DATUM.
          else.
            LD_DATA_LANCTO = TI_400_CONFER-DATA_ACRESC.
          endif.

          clear: TI_VLRCTLGLCTO[].

          " Corrigir Ctg. Lançamento
          if VG_SAVE_OK = 'REVDOCSAP2' and  VG_WA_CONFER-CTLGLANCTO ne 'Q'.
            VG_WA_CONFER-CTLGLANCTO = 'Q'.
          elseif VG_SAVE_OK = 'REVDOCSAP3' and  VG_WA_CONFER-CTLGLANCTO ne 'P'.
            VG_WA_CONFER-CTLGLANCTO = 'P'.
          endif.
          case VG_SAVE_OK.
            when 'REVDOCSAP1'. "Conferência
              if VG_WA_CONFER-CTLGLANCTO = 'VC'.
                clear: VG_SAVE_OK.
                perform GERA_LANCAMENTO_CONFERIDO.
              endif.
            when 'REVDOCSAP2'. "Quebra
              if ( VG_WA_CONFER-CTLGLANCTO = 'Q' or VG_WA_CONFER-CTLGLANCTO = 'S' ).
                clear: VG_SAVE_OK.
                perform GERA_LANCAMENTO_QUEBRA.
              endif.
            when 'REVDOCSAP3'. "Perda
              clear: VG_SAVE_OK.
              perform GERA_LANCAMENTO_PERDA.
          endcase.

          loop at TI_VLRCTLGLCTO.

            clear TI_OBJ_CONTABIL.

*     Gera descrição para o lançamento
            condense TI_VLRCTLGLCTO-CTLGLANCTO.
            concatenate TI_VLRCTLGLCTO-CTLGLANCTO '/' LC_SHTYP into LC_CTLG_TIPTRP.

            if TI_VLRCTLGLCTO-HISTORICO is initial.
              concatenate TI_400_CONFER-DSCODPOSTO
                          'CF:'    TI_400_CONFER-CTAFRETE
                          'DACTE:' TI_400_CONFER-CONHEC
                          into VG_CAMPO_SGTXT separated by SPACE.
            else.
              concatenate TI_VLRCTLGLCTO-HISTORICO
                          'CF:'    TI_400_CONFER-CTAFRETE
                          'DACTE:' TI_400_CONFER-CONHEC into VG_CAMPO_SGTXT separated by SPACE.
            endif.

*     Atualiza tabela contábil
            perform LANCTO_ATLZ_ZIB_CONTABIL using TI_400_CONFER-CODTRP
                                                   TI_400_CONFER-CODPOSTO
                                                   TI_400_CONFER-LOTE
                                                   TI_400_CONFER-CHVID
                                                   LD_DATA_LANCTO
                                                   TI_VLRCTLGLCTO-VALOR
                                                   TI_VLRCTLGLCTO-CTDEBITO
                                                   TI_VLRCTLGLCTO-RAZESP_D
                                                   TI_VLRCTLGLCTO-CHVLCTO_D
                                                   TI_VLRCTLGLCTO-CTCREDITO
                                                   TI_VLRCTLGLCTO-RAZESP_C
                                                   TI_VLRCTLGLCTO-CHVLCTO_C
                                                   TI_VLRCTLGLCTO-ZUONR_D
                                                   TI_VLRCTLGLCTO-ZUONR_C
                                                   VG_CAMPO_SGTXT
                                                   CC_TDOC_CONF
                                                   SY-DATUM
                                                   SPACE " CSB
                                          changing TI_OBJ_CONTABIL-OBJ_KEY
                                                   TI_OBJ_CONTABIL-BUKRS.

*     Salva documentos para geração lançamento
            TI_OBJ_CONTABIL-CODTRP      = TI_400_CONFER-CODTRP.
            TI_OBJ_CONTABIL-CODPOSTO    = TI_400_CONFER-CODPOSTO.
            TI_OBJ_CONTABIL-LOTE        = TI_400_CONFER-LOTE.
            TI_OBJ_CONTABIL-CHVID       = TI_400_CONFER-CHVID.
            TI_OBJ_CONTABIL-VALOR       = TI_VLRCTLGLCTO-VALOR.
            TI_OBJ_CONTABIL-TIPTRANSP   = TI_VLRCTLGLCTO-TIPTRANSP.
            TI_OBJ_CONTABIL-CTLGLANCTO  = TI_VLRCTLGLCTO-CTLGLANCTO.
            TI_OBJ_CONTABIL-DATA_CONFER = LD_DATA_LANCTO.
            TI_OBJ_CONTABIL-OBSERV      = TI_400_CONFER-OBSERVACOES.
            TI_OBJ_CONTABIL-CONHEC      = TI_400_CONFER-CONHEC.
            append TI_OBJ_CONTABIL.

          endloop.

          check: not TI_OBJ_CONTABIL[] is initial.

* Gera documento contábil
*  PERFORM lancto_gera_docto_contabil USING cc_a_confirmar
*                                  CHANGING lf_ha_doc_ok.
*  CHECK: lf_ha_doc_ok = cc_x.

          perform ATLZ_ZLEST0022.

* Atualiza tela de Lote
          if TI_203_LOTES[] is initial.
*   Retorna para nova consulta
            perform RENOVA_TELA_PARA_PESQUISA.
            perform RESETA_VLRSALVO_SELECAO_1200.
          else.
*   Renova Grid
            describe table TI_203_LOTES lines VG_SBS203_TABCONTROL-LINES.
*   Avança para aba de Lote
            VG_DYNNR_TABSTRIP = '0200'.
            VG_MAIN100_TABSTRIP-ACTIVETAB = CC_100_TABSTRIP-TAB1.
          endif.

          exit.
        endif.

        add 1 to VG_INDEX.
        read table TI_COCKPIT_CONFER into VG_WA_CONFER index VG_INDEX.
        if SY-SUBRC <> 0 or
           VG_WA_CONFER-CODTRP   <> TI_400_CONFER-CODTRP   or
           VG_WA_CONFER-CODPOSTO <> TI_400_CONFER-CODPOSTO or
           VG_WA_CONFER-LOTE     <> TI_400_CONFER-LOTE     or
           VG_WA_CONFER-CHVID    <> TI_400_CONFER-CHVID.
          exit.
        endif.
      enddo.
    else.

      if TI_203_LOTES-STATUS eq 'C'.

        if TI_400_CONFER-DATA_ACRESC is initial.
          LD_DATA_LANCTO = SY-DATUM.
        else.
          LD_DATA_LANCTO = TI_400_CONFER-DATA_ACRESC.
        endif.

        clear: TI_VLRCTLGLCTO[].

        case VG_SAVE_OK.
          when 'REVDOCSAP1'. "Conferência
            clear: VG_SAVE_OK.
            perform GERA_LANCAMENTO_CONFERIDO.
          when 'REVDOCSAP2'. "Quebra
            clear: VG_SAVE_OK.
            perform GERA_LANCAMENTO_QUEBRA.
          when 'REVDOCSAP3'. "Perda
            clear: VG_SAVE_OK.
            perform GERA_LANCAMENTO_PERDA.
        endcase.

        loop at TI_VLRCTLGLCTO.

          clear TI_OBJ_CONTABIL.

*     Gera descrição para o lançamento
          condense TI_VLRCTLGLCTO-CTLGLANCTO.
          concatenate TI_VLRCTLGLCTO-CTLGLANCTO '/' LC_SHTYP into LC_CTLG_TIPTRP.

          if TI_VLRCTLGLCTO-HISTORICO is initial.
            concatenate TI_400_CONFER-DSCODPOSTO
                        'CF:'    TI_400_CONFER-CTAFRETE
                        'DACTE:' TI_400_CONFER-CONHEC into VG_CAMPO_SGTXT separated by SPACE.
          else.
            concatenate TI_VLRCTLGLCTO-HISTORICO
                        'CF:'    TI_400_CONFER-CTAFRETE
                        'DACTE:' TI_400_CONFER-CONHEC into VG_CAMPO_SGTXT separated by SPACE.
          endif.

*     Atualiza tabela contábil
          perform LANCTO_ATLZ_ZIB_CONTABIL using TI_400_CONFER-CODTRP
                                                 TI_400_CONFER-CODPOSTO
                                                 TI_400_CONFER-LOTE
                                                 TI_400_CONFER-CHVID
                                                 LD_DATA_LANCTO
                                                 TI_VLRCTLGLCTO-VALOR
                                                 TI_VLRCTLGLCTO-CTDEBITO
                                                 TI_VLRCTLGLCTO-RAZESP_D
                                                 TI_VLRCTLGLCTO-CHVLCTO_D
                                                 TI_VLRCTLGLCTO-CTCREDITO
                                                 TI_VLRCTLGLCTO-RAZESP_C
                                                 TI_VLRCTLGLCTO-CHVLCTO_C
                                                 TI_VLRCTLGLCTO-ZUONR_D
                                                 TI_VLRCTLGLCTO-ZUONR_C
                                                 VG_CAMPO_SGTXT
                                                 CC_TDOC_CONF
                                                 SHEADER_300-DATA_FECHAMENTO
                                                 SPACE "CSB
                                        changing TI_OBJ_CONTABIL-OBJ_KEY
                                                 TI_OBJ_CONTABIL-BUKRS.

*     Salva documentos para geração lançamento
          TI_OBJ_CONTABIL-CODTRP      = TI_400_CONFER-CODTRP.
          TI_OBJ_CONTABIL-CODPOSTO    = TI_400_CONFER-CODPOSTO.
          TI_OBJ_CONTABIL-LOTE        = TI_400_CONFER-LOTE.
          TI_OBJ_CONTABIL-CHVID       = TI_400_CONFER-CHVID.
          TI_OBJ_CONTABIL-VALOR       = TI_VLRCTLGLCTO-VALOR.
          TI_OBJ_CONTABIL-TIPTRANSP   = TI_VLRCTLGLCTO-TIPTRANSP.
          TI_OBJ_CONTABIL-CTLGLANCTO  = TI_VLRCTLGLCTO-CTLGLANCTO.
          TI_OBJ_CONTABIL-DATA_CONFER = LD_DATA_LANCTO.
          TI_OBJ_CONTABIL-OBSERV      = TI_400_CONFER-OBSERVACOES.
          TI_OBJ_CONTABIL-CONHEC      = TI_400_CONFER-CONHEC.
          append TI_OBJ_CONTABIL.

        endloop.

        check: not TI_OBJ_CONTABIL[] is initial.

* Gera documento contábil
*  PERFORM lancto_gera_docto_contabil USING cc_a_confirmar
*                                  CHANGING lf_ha_doc_ok.
*  CHECK: lf_ha_doc_ok = cc_x.

        perform ATLZ_ZLEST0022.

* Atualiza tela de Lote
        if TI_203_LOTES[] is initial.
*   Retorna para nova consulta
          perform RENOVA_TELA_PARA_PESQUISA.
          perform RESETA_VLRSALVO_SELECAO_1200.
        else.
*   Renova Grid
          describe table TI_203_LOTES lines VG_SBS203_TABCONTROL-LINES.
*   Avança para aba de Lote
          VG_DYNNR_TABSTRIP = '0200'.
          VG_MAIN100_TABSTRIP-ACTIVETAB = CC_100_TABSTRIP-TAB1.
        endif.

        exit.
      endif.

    endif.
* Limpando a tela apos o reenviio para o SAP..
    VG_INDEX = VG_SBS400_TABCONTROL-TOP_LINE + ( VG_INDEX - 1 ).
    perform DESMARCAR_LINHAS_TC_401.
    read table TI_400_CONFER index VG_INDEX.
    if SY-SUBRC is initial.
      TI_400_CONFER-MARK = CC_X.
      modify TI_400_CONFER index VG_INDEX transporting MARK.
      VG_SAVE_OK = 'DETLH_401'.
    endif.

    clear: VG_SAVE_OK.

  elseif VG_SAVE_OK = '/CS'. "Duplo Clique

    get cursor line VG_INDEX.
    if VG_SBS400_TABCONTROL-LINES >= VG_INDEX.
      VG_INDEX = VG_SBS400_TABCONTROL-TOP_LINE + ( VG_INDEX - 1 ).
      perform DESMARCAR_LINHAS_TC_401.
      read table TI_400_CONFER index VG_INDEX.
      if SY-SUBRC is initial.
        TI_400_CONFER-MARK = CC_X.
        modify TI_400_CONFER index VG_INDEX transporting MARK.
        VG_SAVE_OK = 'DETLH_401'.
      endif.
    endif.

  endif.

endmodule.                 " CHECK_OKCODE_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_401  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_401 input.

* Determina função a ser executada
  case VG_SAVE_OK.

    when 'EDTLANCTO_401'.
      perform EDITAR_ENTRADAS_DOCTO_401.
    when 'LCCONTB_401'.
      perform GERAR_LANCTO_CONTABIL_401.
    when 'NOVOS_401'.
      perform NOVAS_ENTRADAS_DOCTO_401.
    when 'DELIN_401'.
      perform ELIMINA_LINHAS_TC_401.
    when 'DMARKALL_401'.
      perform DESMARCAR_LINHAS_TC_401.
    when 'DETLH_401'.
      perform EXIBE_DETALHE_DOCTO_401.
    when 'EXCEL_401'.
      perform GERA_EXCEL_401.
    when 'BTNANULAR'.
      clear: VG_SAVE_OK.
      perform GERA_ANULACAO_404.
    when 'BTNLISTAR'.
      clear: VG_SAVE_OK.
      perform LISTAR_ANULACAO_404.
  endcase.

* Limpa controle de funcionalidade processada
  clear: VG_SAVE_OK.

endmodule.                 " CHECK_OKCODE_401  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHAMA_FB03_DOCSAP_203  INPUT
*&---------------------------------------------------------------------*
module CHAMA_FB03_DOCSAP_203 input.

  check: not TI_203_LOTES-DOCSAPADTO is initial.

  set parameter id: 'BLN' field TI_203_LOTES-DOCSAPADTO,
                    'BUK' field TI_203_LOTES-BUKRS,
                    'GJR' field TI_203_LOTES-GJAHR.

* Visualiza documento contábil
  call transaction 'FB03' and skip first screen.

  wait up to 2 seconds.
* ---> S4 Migration - 14/06/2023 - MA
*  SELECT SINGLE * INTO VG_ZFBDT
*    FROM BSEG
*   WHERE BUKRS EQ TI_203_LOTES-BUKRS
*     AND GJAHR EQ TI_203_LOTES-GJAHR
*     AND BELNR EQ TI_203_LOTES-DOCSAPADTO
*     AND BSCHL EQ '39'.
  data: LT_BSEG  type FAGL_T_BSEG,
        LV_BELNR type BELNR_D.

  LV_BELNR = conv #( TI_203_LOTES-DOCSAPADTO ).

  call function 'FAGL_GET_BSEG'
    exporting
      I_BUKRS   = TI_203_LOTES-BUKRS
      I_BELNR   = LV_BELNR
      I_GJAHR   = TI_203_LOTES-GJAHR
    importing
      ET_BSEG   = LT_BSEG
    exceptions
      NOT_FOUND = 1
      others    = 2.

  delete LT_BSEG where BSCHL ne '39'.

  read table LT_BSEG into data(LS_BSEG) index 1.
  if SY-SUBRC = 0.
    move-corresponding LS_BSEG to VG_ZFBDT.
  endif.
*<--- S4 Migration - 14/06/2023 - MA

  if SY-SUBRC <> 0.
* Implement suitable error handling here
  endif.

* ---> S4 Migration - 14/06/2023 - MA
  if ( SY-SUBRC eq 0 ) and ( VG_ZFBDT-ZFBDT ne TI_203_LOTES-VENCIMENTO ) and ( VG_ZFBDT-AUGBL is initial ).

    update BSEG
       set ZFBDT = VG_ZFBDT-ZFBDT
     where BUKRS eq TI_203_LOTES-BUKRS
       and GJAHR eq TI_203_LOTES-GJAHR
       and BELNR eq TI_203_LOTES-DOCSAPADTO
       and BSCHL ne '39'
       and AUGBL eq ''.

    update ZLEST0015
       set VENCIMENTO = VG_ZFBDT-ZFBDT
     where TRANSPORTADOR = TI_203_LOTES-CODTRP
       and POSTO         = TI_203_LOTES-CODPOSTO
       and LOTE          = TI_203_LOTES-LOTE
       and DOCSAP        = TI_203_LOTES-DOCSAPADTO
       and GJAHR         = TI_203_LOTES-GJAHR
       and BUKRS         = TI_203_LOTES-BUKRS.

    commit work.

    perform NOVOS_CONHECIMENTOS_200.

  endif.

endmodule.                 " CHAMA_FB03_DOCSAP_203  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHAMA_VT03_TKNUM_300  INPUT
*&---------------------------------------------------------------------*
module CHAMA_VT03_TKNUM_300 input.

  check: not TI_300_LACTO-CONHEC is initial.

  perform CHAMA_TRANSACAO_VT03N using TI_300_LACTO-CODTRP
                                      TI_300_LACTO-CONHEC
                                      TI_300_LACTO-CTAFRETE.

endmodule.                 " CHAMA_VT03_TKNUM_300  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHAMA_VT03_TKNUM_400  INPUT
*&---------------------------------------------------------------------*
module CHAMA_VT03_TKNUM_400 input.

  check: not TI_400_CONFER-CONHEC is initial.

  perform CHAMA_TRANSACAO_VT03N using TI_400_CONFER-CODTRP
                                      TI_400_CONFER-CONHEC
                                      TI_400_CONFER-CTAFRETE.

endmodule.                 " CHAMA_VT03_TKNUM_300  INPUT

*&---------------------------------------------------------------------*
*&      Module  ALTERA_ITEM_MARCADO_400  INPUT
*&---------------------------------------------------------------------*
module ALTERA_ITEM_MARCADO_400 input.

  modify TI_400_CONFER index VG_SBS400_TABCONTROL-CURRENT_LINE
                       transporting CHECK.

endmodule.                 " ALTERA_ITEM_MARCADO_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHAMA_FB03_DOCSAP_400  INPUT
*&---------------------------------------------------------------------*
module CHAMA_FB03_DOCSAP_400 input.

* Lê a primeira ocorrênica marcada para detalhamento
  read table TI_400_CONFER with key MARK = CC_X.
  check: SY-SUBRC is initial.

  get cursor field VG_CAMPO_CHAR40.

  assign (VG_CAMPO_CHAR40) to <DOCSAP>.
  check: <DOCSAP> is assigned.

  check: <DOCSAP> is not initial.

  read table TI_COCKPIT_CONFER into VG_WA_CONFER
                           with key CODTRP   = TI_400_CONFER-CODTRP
                                    CODPOSTO = TI_400_CONFER-CODPOSTO
                                    LOTE     = TI_400_CONFER-LOTE
                                    CHVID    = TI_400_CONFER-CHVID
                                    DOCSAP   = <DOCSAP>.
  check: SY-SUBRC is initial.

  set parameter id: 'BLN' field VG_WA_CONFER-DOCSAP,
                    'BUK' field VG_WA_CONFER-BUKRS,
                    'GJR' field VG_WA_CONFER-GJAHR.

* Visualiza documento contábil
  call transaction 'FB03' and skip first screen.

endmodule.                 " CHAMA_FB03_DOCSAP_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_402  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_402 input.

  case OK_CODE.

    when 'CANCEL'.

*     Restaura valores anteriores no cancelamento para edição do lançto
      if SCTRL_SALDO_400-EDIT_LANC = CC_X.
        SCTRL_SALDO_400-SALDO_ATUAL = SCTRL_SALDO_400-SALDO_HIST.
        SDETLH_CONFER_400-SALDO_HISTORICO = SCTRL_SALDO_400-SALDO_ATUAL.
      endif.

      set screen 0. leave screen.

    when 'SAVE'.

      perform SALVA_LANCTO_MANUAL_402.
      set screen 0. leave screen.

    when others.

  endcase.

  clear OK_CODE.

endmodule.                 " CHECK_OKCODE_402  INPUT

*&---------------------------------------------------------------------*
*&      Module  MATCH_CODE_CHVID_400  INPUT
*&---------------------------------------------------------------------*
module MATCH_CODE_CHVID_400 input.

  perform EXIBE_MATCH_CODE_CHVID using '0402'
                                       'SNLANCTO_CONFER_400-CHVID'
                                       'SNLANCTO_CONFER_400-DESCHVID'.

endmodule.                 " MATCH_CODE_CHVID_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_CHVID_400  INPUT
*&---------------------------------------------------------------------*
module CHECK_CHVID_400 input.

  clear VG_MSGERRO.

* Valida chave de Histórico
  select single *
    from ZLEST0025
   where CHVID = SNLANCTO_CONFER_400-CHVID.

* Analisa resultado
  if SY-SUBRC <> 0.
    message W037 with SNLANCTO_CONFER_400-CHVID into VG_MSGERRO.
  elseif ZLEST0025-BL = CC_BLOQUEADO.
    message W038 with SNLANCTO_CONFER_400-CHVID into VG_MSGERRO.
  elseif ZLEST0025-LCTOCHVID <> CC_CTRL_MANUAL.
    VG_MSGERRO = text-M14.
  elseif ZLEST0025-NATUREZACHVID = 'S' and SCTRL_SALDO_400-SALDO_ATUAL > 0.
    VG_MSGERRO = text-M52.
  elseif ZLEST0025-NATUREZACHVID = 'H' and SCTRL_SALDO_400-SALDO_ATUAL < 0.
    VG_MSGERRO = text-M52.
  endif.

* Verifica se já existe um mesmo ChvId em lançamento posterior
  if VG_MSGERRO is initial.
    read table TI_400_CONFER
    with key CHVID = SNLANCTO_CONFER_400-CHVID transporting no fields.
    if SY-SUBRC is initial.
      VG_MSGERRO = text-M19.
    endif.
  endif.

* Exibe mensagem de erro
  check: not VG_MSGERRO is initial.
  message VG_MSGERRO type CC_E.

endmodule.                 " CHECK_CHVID_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_VALOR_400  INPUT
*&---------------------------------------------------------------------*
module CHECK_VALOR_400 input.

*  vg_campo_curr = ABS( sctrl_saldo_400-saldo_atual ).
* Comentado a pedido da jacqueline 08.01.2011
** Analisa se o valor se pode ser lançado
*  IF snlancto_confer_400-valor > vg_campo_curr.
*
*    vg_msgerro = text-m15.
*
*    WRITE snlancto_confer_400-valor TO vg_campo_char20 NO-ZERO.
*    CONDENSE vg_campo_char20.
*    REPLACE FIRST OCCURRENCE OF '&1' IN vg_msgerro
*            WITH vg_campo_char20.
*
*    WRITE vg_campo_curr TO vg_campo_char20 NO-ZERO.
*    CONDENSE vg_campo_char20.
*    REPLACE FIRST OCCURRENCE OF '&2' IN vg_msgerro
*            WITH vg_campo_char20.
*    MESSAGE vg_msgerro TYPE cc_e.
*
*  ENDIF.

endmodule.                 " CHECK_VALOR_400  INPUT

*&---------------------------------------------------------------------*
*&      Module  MATCH_CODE_BL_203  INPUT
*&---------------------------------------------------------------------*
module MATCH_CODE_BL_203 input.

  perform EXIBE_MATCH_CODE_DOMINIO using 'ZBL'
                                changing VG_CAMPO_CHAR05.

endmodule.                 " MATCH_CODE_BL_203  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_403  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_403 input.

  if OK_CODE = 'PICK'.
    perform ALTERA_PESO_CONFIRMADO_COCKPIT.
  endif.

  clear OK_CODE.

* Encerra processamento do pop-up
  set screen 0.
  leave screen.

endmodule.                 " CHECK_OKCODE_403  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_OKCODE_204  INPUT
*&---------------------------------------------------------------------*
module CHECK_OKCODE_204 input.

  data: VG_TABIX     type SY-TABIX.

  loop at TI_204D_ACRDECR.
    VG_TABIX = SY-TABIX.
    read table TI_204H_ACRDECR with key LOTE = TI_204D_ACRDECR-LOTE_APLICAR ACAO = CC_1_ADACDC.
    if not SY-SUBRC is initial.
      clear TI_204D_ACRDECR-LOTE_APLICAR.
      modify TI_204D_ACRDECR index VG_TABIX transporting LOTE_APLICAR.
    endif.
  endloop.

  loop at TI_204H_ACRDECR.
    VG_TABIX = SY-TABIX.
    VG_CAMPO_CURR = 0.

    loop at TI_204D_ACRDECR into VG_WA_ACRDECR where CODTRP       eq TI_204H_ACRDECR-CODTRP
                                                 and CODPOSTO     eq TI_204H_ACRDECR-CODPOSTO
                                                 and LOTE_APLICAR eq TI_204H_ACRDECR-LOTE.
      if ( VG_WA_ACRDECR-CHVID eq '18' ) or ( VG_WA_ACRDECR-CHVID eq '26' ) or ( VG_WA_ACRDECR-CHVID eq '28' ).
        multiply VG_WA_ACRDECR-WRBTR by -1.
      endif.
      add VG_WA_ACRDECR-WRBTR to VG_CAMPO_CURR.
    endloop.
    TI_204H_ACRDECR-VLRLANCTO = TI_204H_ACRDECR-VLRREALIZADO + VG_CAMPO_CURR.
    modify TI_204H_ACRDECR index VG_TABIX transporting VLRLANCTO.
  endloop.

  if OK_CODE = 'APLACDC'.
    VG_RESPOSTA = CC_X.
    clear OK_CODE.
    set screen 0.
    leave screen.
  elseif OK_CODE = 'CANC'.
    clear: OK_CODE,
           VG_RESPOSTA.
    set screen 0.
    leave screen.
  endif.

endmodule.                 " CHECK_OKCODE_204  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_204H  INPUT
*&---------------------------------------------------------------------*
module READ_TABLE_CONTROL_204H input.


* Ação em branco são tratadas como postergar, ou adiar acréscimo e decréscimo
* Caso queira consistir exite a mensagem de texto text-m36 que pode ser usada

  if TI_204H_ACRDECR-ACAO = CC_1_ADACDC and
     TI_204H_ACRDECR-VLRLANCTO <= 0.
    message text-M35 type CC_E.
  else.
    modify TI_204H_ACRDECR index VG_SBS204H_TABCONTROL-CURRENT_LINE
                           transporting ACAO.
  endif.

endmodule.                 " READ_TABLE_CONTROL_204H  INPUT

*&---------------------------------------------------------------------*
*&      Module  PREENCHE_VLRS_ACAO_204  INPUT
*&---------------------------------------------------------------------*
module PREENCHE_VLRS_ACAO_204 input.

  perform PREENCHE_VALORES_ACAO_ACRDESC.

endmodule.                 " PREENCHE_VLRS_ACAO_204  INPUT

*&---------------------------------------------------------------------*
*&      Form  GERA_LANCAMENTO_PERDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GERA_LANCAMENTO_PERDA .

  data: LC_SHTYP         type SHTYP,
        WA_ZLEST0022     type ZLEST0022,
        VG_TRANSPORTADOR type TDLNR,
        VG_POSTO         type LIFNR,
        VG_LOTE          type CHAR10.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-LOTE
    importing
      OUTPUT = VG_LOTE.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODPOSTO
    importing
      OUTPUT = VG_POSTO.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODTRP
    importing
      OUTPUT = VG_TRANSPORTADOR.

  select single * into WA_ZLEST0022
    from ZLEST0022
   where TRANSPORTADOR  eq VG_TRANSPORTADOR
     and POSTO          eq VG_POSTO
     and LOTE           eq VG_LOTE
     and CHVID          eq TI_400_CONFER-CHVID
     and CTLGLANCTO     eq CC_CTLG_PERDA
     and CONHECIMENTO   eq TI_400_CONFER-CONHEC.

  if SY-SUBRC is initial and SDETLH_CONFER_400-DOCSAP3 is not initial.
    message W052 with TI_400_CONFER-LOTE TI_400_CONFER-CONHEC.
  else.
    SDETLH_CONFER_400-DOC3MSG = 'Processando'.
    select SHTYP
      into LC_SHTYP
      from VTTK
        up to 1 rows
     where TDLNR = TI_400_CONFER-CODTRP
       and EXTI1 = TI_400_CONFER-CONHEC
       and EXTI2 = TI_400_CONFER-CTAFRETE.
    endselect.

    if SDETLH_CONFER_400-VLR_SOBRA_QUEBRA <> 0.

      perform OBTEM_CTRAZAO_CATLGLCTO_401  using TI_400_CONFER-CODTRP
                                                 TI_400_CONFER-CODPOSTO
                                                 TI_400_CONFER-CONHEC
                                                 TI_400_CONFER-CTAFRETE
                                                 TI_400_CONFER-CHVID
                                                 LC_SHTYP
                                                 CC_CTLG_PERDA
                                                 SPACE
                                        changing ZLEST0018.

      if not ZLEST0018-CONTADEBITO  is initial and
         not ZLEST0018-CONTACREDITO is initial.
        TI_VLRCTLGLCTO-CTLGLANCTO  = CC_CTLG_PERDA.
        TI_VLRCTLGLCTO-TIPTRANSP   = LC_SHTYP.
        TI_VLRCTLGLCTO-CTDEBITO    = ZLEST0018-CONTADEBITO.
        TI_VLRCTLGLCTO-RAZESP_D    = ZLEST0018-RAZAOESP_D.
        TI_VLRCTLGLCTO-CHVLCTO_D   = ZLEST0018-CHVLANCTO_D.
        TI_VLRCTLGLCTO-CTCREDITO   = ZLEST0018-CONTACREDITO.
        TI_VLRCTLGLCTO-RAZESP_C    = ZLEST0018-RAZAOESP_C.
        TI_VLRCTLGLCTO-CHVLCTO_C   = ZLEST0018-CHVLANCTO_C.
        TI_VLRCTLGLCTO-TIPOCONTA_C = ZLEST0018-TIPOCONTA_C.
        TI_VLRCTLGLCTO-TIPOCONTA_D = ZLEST0018-TIPOCONTA_D.
        TI_VLRCTLGLCTO-VALOR       = SDETLH_CONFER_400-VLR_SOBRA_QUEBRA.
        TI_VLRCTLGLCTO-HISTORICO   = ZLEST0018-ESPECIFICACAO.

        if TI_VLRCTLGLCTO-RAZESP_D is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
        else.
          case TI_400_CONFER-CHVID.
            when '18' or '20'.
              concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
            when '17'.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
            when others.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
          endcase.
        endif.

        if TI_VLRCTLGLCTO-RAZESP_C is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_C.
        else.
          concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_C.
        endif.

        append TI_VLRCTLGLCTO.
      endif.
    endif.

  endif.

endform.                    " GERA_LANCAMENTO_PERDA

*&---------------------------------------------------------------------*
*&      Form  GERA_LANCAMENTO_QUEBRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GERA_LANCAMENTO_QUEBRA .

  data: LC_SHTYP         type SHTYP,
        WA_ZLEST0022     type ZLEST0022,
        VG_TRANSPORTADOR type TDLNR,
        VG_POSTO         type LIFNR,
        VG_LOTE          type CHAR10.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-LOTE
    importing
      OUTPUT = VG_LOTE.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODPOSTO
    importing
      OUTPUT = VG_POSTO.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODTRP
    importing
      OUTPUT = VG_TRANSPORTADOR.

  perform OBTEM_VLRS_P_Q_S_VC_VP_401 using LC_SHTYP
                                  changing VG_VLR_PERDA
                                           VG_VLR_QUEBRA
                                           VG_VLR_SOBRA
                                           VG_VLR_CONFER
                                           VG_VLR_PROGRAMADO.

  select SHTYP
    into LC_SHTYP
    from VTTK
      up to 1 rows
   where TDLNR = TI_400_CONFER-CODTRP
     and EXTI1 = TI_400_CONFER-CONHEC
     and EXTI2 = TI_400_CONFER-CTAFRETE.
  endselect.

  if VG_VLR_QUEBRA <> 0.

    select single * into WA_ZLEST0022
      from ZLEST0022
     where TRANSPORTADOR  eq VG_TRANSPORTADOR
       and POSTO          eq VG_POSTO
       and LOTE           eq VG_LOTE
       and CHVID          eq TI_400_CONFER-CHVID
       and CTLGLANCTO     eq CC_CTLG_QUEBRA
       and CONHECIMENTO   eq TI_400_CONFER-CONHEC.

    if SY-SUBRC is initial and SDETLH_CONFER_400-DOCSAP2 is not initial.
      message W051 with TI_400_CONFER-LOTE TI_400_CONFER-CONHEC.
    else.
      SDETLH_CONFER_400-DOC2MSG = 'Processando'.
      perform OBTEM_CTRAZAO_CATLGLCTO_401  using TI_400_CONFER-CODTRP
                                                 TI_400_CONFER-CODPOSTO
                                                 TI_400_CONFER-CONHEC
                                                 TI_400_CONFER-CTAFRETE
                                                 TI_400_CONFER-CHVID
                                                 LC_SHTYP
                                                 CC_CTLG_QUEBRA
                                                 SPACE
                                        changing ZLEST0018.

      if not ZLEST0018-CONTADEBITO  is initial and
         not ZLEST0018-CONTACREDITO is initial.
        TI_VLRCTLGLCTO-CTLGLANCTO  = CC_CTLG_QUEBRA.
        TI_VLRCTLGLCTO-TIPTRANSP   = LC_SHTYP.
        TI_VLRCTLGLCTO-CTDEBITO    = ZLEST0018-CONTADEBITO.
        TI_VLRCTLGLCTO-RAZESP_D    = ZLEST0018-RAZAOESP_D.
        TI_VLRCTLGLCTO-CHVLCTO_D   = ZLEST0018-CHVLANCTO_D.
        TI_VLRCTLGLCTO-CTCREDITO   = ZLEST0018-CONTACREDITO.
        TI_VLRCTLGLCTO-RAZESP_C    = ZLEST0018-RAZAOESP_C.
        TI_VLRCTLGLCTO-CHVLCTO_C   = ZLEST0018-CHVLANCTO_C.
        TI_VLRCTLGLCTO-TIPOCONTA_C = ZLEST0018-TIPOCONTA_C.
        TI_VLRCTLGLCTO-TIPOCONTA_D = ZLEST0018-TIPOCONTA_D.
        TI_VLRCTLGLCTO-VALOR       = VG_VLR_QUEBRA.
        TI_VLRCTLGLCTO-HISTORICO   = ZLEST0018-ESPECIFICACAO.

        if TI_VLRCTLGLCTO-RAZESP_D is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
        else.
          case TI_400_CONFER-CHVID.
            when '18' or '20'.
              concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
            when '17'.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
            when others.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
          endcase.
        endif.

        if TI_VLRCTLGLCTO-RAZESP_C is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_C.
        else.
          concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_C.
        endif.

        append TI_VLRCTLGLCTO.
      endif.
    endif.
  endif.

  if VG_VLR_SOBRA <> 0.

    select single * into WA_ZLEST0022
      from ZLEST0022
     where TRANSPORTADOR  eq VG_TRANSPORTADOR
       and POSTO          eq VG_POSTO
       and LOTE           eq VG_LOTE
       and CHVID          eq TI_400_CONFER-CHVID
       and CTLGLANCTO     eq CC_CTLG_SOBRA
       and CONHECIMENTO   eq TI_400_CONFER-CONHEC.

    if SY-SUBRC is initial.
      message W053 with TI_400_CONFER-LOTE TI_400_CONFER-CONHEC.
    else.

      perform OBTEM_CTRAZAO_CATLGLCTO_401  using TI_400_CONFER-CODTRP
                                                 TI_400_CONFER-CODPOSTO
                                                 TI_400_CONFER-CONHEC
                                                 TI_400_CONFER-CTAFRETE
                                                 TI_400_CONFER-CHVID
                                                 LC_SHTYP
                                                 CC_CTLG_SOBRA
                                                 SPACE
                                        changing ZLEST0018.

      if not ZLEST0018-CONTADEBITO  is initial and
         not ZLEST0018-CONTACREDITO is initial.
        TI_VLRCTLGLCTO-CTLGLANCTO  = CC_CTLG_SOBRA.
        TI_VLRCTLGLCTO-TIPTRANSP   = LC_SHTYP.
        TI_VLRCTLGLCTO-CTDEBITO    = ZLEST0018-CONTADEBITO.
        TI_VLRCTLGLCTO-RAZESP_D    = ZLEST0018-RAZAOESP_D.
        TI_VLRCTLGLCTO-CHVLCTO_D   = ZLEST0018-CHVLANCTO_D.
        TI_VLRCTLGLCTO-CTCREDITO   = ZLEST0018-CONTACREDITO.
        TI_VLRCTLGLCTO-RAZESP_C    = ZLEST0018-RAZAOESP_C.
        TI_VLRCTLGLCTO-CHVLCTO_C   = ZLEST0018-CHVLANCTO_C.
        TI_VLRCTLGLCTO-TIPOCONTA_C = ZLEST0018-TIPOCONTA_C.
        TI_VLRCTLGLCTO-TIPOCONTA_D = ZLEST0018-TIPOCONTA_D.
        TI_VLRCTLGLCTO-VALOR       = VG_VLR_SOBRA.
        TI_VLRCTLGLCTO-HISTORICO   = ZLEST0018-ESPECIFICACAO.

        if TI_VLRCTLGLCTO-RAZESP_D is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
        else.
          case TI_400_CONFER-CHVID.
            when '18' or '20'.
              concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
            when '17'.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
            when others.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
          endcase.
        endif.

        if TI_VLRCTLGLCTO-RAZESP_C is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_C.
        else.
          concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_C.
        endif.

        append TI_VLRCTLGLCTO.
      endif.
    endif.
  endif.

endform.                    " GERA_LANCAMENTO_QUEBRA

*&---------------------------------------------------------------------*
*&      Form  GERA_LANCAMENTO_CONFERIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GERA_LANCAMENTO_CONFERIDO .

  data: LC_SHTYP         type SHTYP,
        WA_ZLEST0022     type ZLEST0022,
        VG_TRANSPORTADOR type TDLNR,
        VG_POSTO         type LIFNR,
        VG_LOTE          type CHAR10.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-LOTE
    importing
      OUTPUT = VG_LOTE.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODPOSTO
    importing
      OUTPUT = VG_POSTO.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      INPUT  = TI_400_CONFER-CODTRP
    importing
      OUTPUT = VG_TRANSPORTADOR.

  select single * into WA_ZLEST0022
    from ZLEST0022
   where TRANSPORTADOR  eq VG_TRANSPORTADOR
     and POSTO          eq VG_POSTO
     and LOTE           eq VG_LOTE
     and CHVID          eq TI_400_CONFER-CHVID
     and CTLGLANCTO     eq CC_CTLG_VLRCONFER
     and CONHECIMENTO   eq TI_400_CONFER-CONHEC.

  if SY-SUBRC is initial and SDETLH_CONFER_400-DOCSAP1 is not initial.
    message W050 with TI_400_CONFER-LOTE TI_400_CONFER-CONHEC.
  else.

    SDETLH_CONFER_400-DOC1MSG = 'Processando'.
    select SHTYP
      into LC_SHTYP
      from VTTK
        up to 1 rows
     where TDLNR = TI_400_CONFER-CODTRP
       and EXTI1 = TI_400_CONFER-CONHEC
       and EXTI2 = TI_400_CONFER-CTAFRETE.
    endselect.

    if SDETLH_CONFER_400-VLR_CONFERIDO <> 0.

      perform OBTEM_CTRAZAO_CATLGLCTO_401  using TI_400_CONFER-CODTRP
                                                 TI_400_CONFER-CODPOSTO
                                                 TI_400_CONFER-CONHEC
                                                 TI_400_CONFER-CTAFRETE
                                                 TI_400_CONFER-CHVID
                                                 LC_SHTYP
                                                 CC_CTLG_VLRCONFER
                                                 SPACE
                                        changing ZLEST0018.
      if not ZLEST0018-CONTADEBITO  is initial and
         not ZLEST0018-CONTACREDITO is initial.
        TI_VLRCTLGLCTO-CTLGLANCTO  = CC_CTLG_VLRCONFER.
        TI_VLRCTLGLCTO-TIPTRANSP   = LC_SHTYP.
        TI_VLRCTLGLCTO-CTDEBITO    = ZLEST0018-CONTADEBITO.
        TI_VLRCTLGLCTO-RAZESP_D    = ZLEST0018-RAZAOESP_D.
        TI_VLRCTLGLCTO-CHVLCTO_D   = ZLEST0018-CHVLANCTO_D.
        TI_VLRCTLGLCTO-CTCREDITO   = ZLEST0018-CONTACREDITO.
        TI_VLRCTLGLCTO-RAZESP_C    = ZLEST0018-RAZAOESP_C.
        TI_VLRCTLGLCTO-CHVLCTO_C   = ZLEST0018-CHVLANCTO_C.
        TI_VLRCTLGLCTO-TIPOCONTA_C = ZLEST0018-TIPOCONTA_C.
        TI_VLRCTLGLCTO-TIPOCONTA_D = ZLEST0018-TIPOCONTA_D.
        TI_VLRCTLGLCTO-VALOR       = SDETLH_CONFER_400-VLR_CONFERIDO.
        TI_VLRCTLGLCTO-HISTORICO   = ZLEST0018-ESPECIFICACAO.

        if TI_VLRCTLGLCTO-RAZESP_D is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
        else.
          case TI_400_CONFER-CHVID.
            when '18' or '20'.
              concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_D.
            when '17'.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
            when others.
              concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_D.
          endcase.
        endif.

        if TI_VLRCTLGLCTO-RAZESP_C is initial.
          concatenate 'FR-' TI_400_CONFER-CTAFRETE into TI_VLRCTLGLCTO-ZUONR_C.
        else.
          concatenate 'FR-' TI_400_CONFER-LOTE into TI_VLRCTLGLCTO-ZUONR_C.
        endif.

        append TI_VLRCTLGLCTO.
      endif.

    endif.
  endif.
endform.                    " GERA_LANCAMENTO_CONFERIDO

*&---------------------------------------------------------------------*
*&      Form  ATLZ_ZLEST0022
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form ATLZ_ZLEST0022 .

  sort TI_OBJ_CONTABIL by CODTRP CODPOSTO LOTE CHVID
                          CONHEC TIPTRANSP CTLGLANCTO.

  loop at TI_400_CONFER.

*   Obtem documento contabil para ChvId
    read table TI_OBJ_CONTABIL with key CODTRP   = TI_400_CONFER-CODTRP
                                        CODPOSTO = TI_400_CONFER-CODPOSTO
                                        LOTE     = TI_400_CONFER-LOTE
                                        CHVID    = TI_400_CONFER-CHVID
                                        CONHEC   = TI_400_CONFER-CONHEC
                               binary search.
    check: SY-SUBRC is initial.
    VG_INDEX = SY-TABIX.

*   Grava documentos contábil gerado no lançamento
    clear ZLEST0022.
    move: TI_400_CONFER-CODTRP       to ZLEST0022-TRANSPORTADOR,
          TI_400_CONFER-CODPOSTO     to ZLEST0022-POSTO,
          TI_400_CONFER-LOTE         to ZLEST0022-LOTE,
          TI_400_CONFER-CHVID        to ZLEST0022-CHVID,
          TI_400_CONFER-OBSERVACOES  to ZLEST0022-OBS_CONFER,
          TI_OBJ_CONTABIL-TIPTRANSP  to ZLEST0022-TIPTRANSP,
          TI_OBJ_CONTABIL-CTLGLANCTO to ZLEST0022-CTLGLANCTO,
          TI_OBJ_CONTABIL-DOCSAP     to ZLEST0022-DOCSAP,
          TI_OBJ_CONTABIL-GJAHR      to ZLEST0022-GJAHR,
          TI_OBJ_CONTABIL-BUKRS      to ZLEST0022-BUKRS,
          TI_OBJ_CONTABIL-OBJ_KEY    to ZLEST0022-OBJ_KEY,
          TI_400_CONFER-CONHEC       to ZLEST0022-CONHECIMENTO,
          TI_400_CONFER-CTAFRETE     to ZLEST0022-CTAFRETE,
          SY-DATUM                   to ZLEST0022-ERDAT,
          SY-UNAME                   to ZLEST0022-UNAME.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = ZLEST0022-TRANSPORTADOR
      importing
        OUTPUT = ZLEST0022-TRANSPORTADOR.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = ZLEST0022-POSTO
      importing
        OUTPUT = ZLEST0022-POSTO.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = ZLEST0022-LOTE
      importing
        OUTPUT = ZLEST0022-LOTE.

    modify ZLEST0022 from ZLEST0022.

  endloop.

* Reseta valores de conferência
  clear: SDETLH_CONFER_400,
         SCTRL_SALDO_400.

endform.                    " ATLZ_ZLEST0022

*&---------------------------------------------------------------------*
*&      Form  LIBERA_LOTES_USUARIO
*&---------------------------------------------------------------------*
*       Libera todos os bloqueios de lote
*----------------------------------------------------------------------*
form LIBERA_LOTES_USUARIO .

  data: begin of WA_ENQ.
          include structure SEQG7.
  data: end of WA_ENQ.

  data: GARG like SEQG3-GARG,
        ENQ  like standard table of WA_ENQ.

  concatenate SY-MANDT WA_203_LOTES-CODTRP WA_203_LOTES-CODPOSTO WA_203_LOTES-LOTE into GARG.

  call function 'ENQUE_READ2'
    exporting
      GNAME = 'ZLEST0015'
    tables
      ENQ   = ENQ.

  loop at ENQ into WA_ENQ.

    call function 'ZDEQUEUE_ZLOTE_POSTOS'
      exporting
        TRANSPORTADOR = WA_ENQ-GARG+03(10)
        POSTO         = WA_ENQ-GARG+13(10)
        LOTE          = WA_ENQ-GARG+23(10).

  endloop.

endform.                    " LIBERA_LOTES_USUARIO

*&---------------------------------------------------------------------*
*&      Module  PREENCHE_LOTES_204  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module PREENCHE_LOTES_204 input.

  data: LC_NAME_ID type   VRM_ID,
        LT_LIST    type   VRM_VALUES,
        LW_VALUE   like   line of LT_LIST.

  refresh LT_LIST.
  clear LW_VALUE.

  loop at TI_204H_ACRDECR where ACAO eq CC_1_ADACDC.
    LW_VALUE-KEY  = TI_204H_ACRDECR-LOTE.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        INPUT  = TI_204H_ACRDECR-LOTE
      importing
        OUTPUT = TI_204H_ACRDECR-LOTE.

    LW_VALUE-TEXT = TI_204H_ACRDECR-LOTE.
    append LW_VALUE to LT_LIST.
  endloop.

  LW_VALUE-KEY = ' '.
  LW_VALUE-TEXT = SPACE.
  append LW_VALUE to LT_LIST.

  LC_NAME_ID = 'TI_204D_ACRDECR-LOTE_APLICAR'.

  call function 'VRM_SET_VALUES'
    exporting
      ID     = LC_NAME_ID
      VALUES = LT_LIST.

endmodule.                 " PREENCHE_LOTES_204  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_204D  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module READ_TABLE_CONTROL_204D input.
  read table TI_204H_ACRDECR with key LOTE = TI_204D_ACRDECR-LOTE_APLICAR ACAO = CC_1_ADACDC.
  if not SY-SUBRC is initial.
    clear TI_204D_ACRDECR-LOTE_APLICAR.
    modify TI_204D_ACRDECR index VG_SBS204D_TABCONTROL-CURRENT_LINE transporting LOTE_APLICAR.
  else.
    modify TI_204D_ACRDECR index VG_SBS204D_TABCONTROL-CURRENT_LINE transporting LOTE_APLICAR.
  endif.
endmodule.                 " READ_TABLE_CONTROL_204D  INPUT
