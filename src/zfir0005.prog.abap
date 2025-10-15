*&--------------------------------------------------------------------&*
*&                     Programa Módulo - FI                           &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 06/09/2024                                              &*
*& Descrição: Mapa de Comprovação de Investimentos                    &*
*& Transação: Relatóio para JOB                                       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Ninjas Evolution                                        &*
*& Código Espec.Funcional/Técnica: 74935                              &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Report ZFIR0005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zfir0005.

*======================================================================*
* Declarações
*======================================================================*
tables: aufk, bkpf, tbtcjob.

types:
  begin of ty_aufnr,
    sign   type char1,
    option type char2,
    low    type aufnr,
    high   type aufnr,
  end of ty_aufnr .

data:
  lt_aufnr  type table of ty_aufnr,
  lv_index  type sy-tabix,
  lv_ultimo type sy-tabix.

data: go_alv        type ref to cl_salv_table,
      lr_columns    type ref to cl_salv_columns_table,
      lr_column     type ref to cl_salv_column_table,
      lr_functions  type ref to cl_salv_functions_list,
      gr_display    type ref to cl_salv_display_settings,
      gr_selections type ref to cl_salv_selections,
      it_ordens     type zttsys_aufnrl,
      wa_ordens     type zsys_aufnr,
      it_conf       type table of zfit0012,
      lv_message    type string.

start-of-selection.
*======================================================================*
*  TELA DE SELEÇÃO
*======================================================================*
  selection-screen: begin of block b1 with frame title text-001.
    select-options:
                    s_kokrs   for  aufk-kokrs no intervals no-extension, "OBLIGATORY
                    s_monat   for  bkpf-monat no intervals no-extension,
                    s_gjahr   for  bkpf-gjahr no intervals no-extension,
                    s_aufnr   for  aufk-aufnr obligatory no intervals,
                    s_prot    for  tbtcjob-jobname no intervals no-extension.
  selection-screen: end of block b1.

*======================================================================*
* Lógica Principal
*======================================================================*

  try.
*======================================================================*
* Busca informações relevantes para o processo
*======================================================================*
      "MESSAGE 'Step 1 - Iniciando job...' TYPE 'S'.

      data(go) = new zcl_int_ib_fi_prot_mcomp_inv( ).

      if s_kokrs[] is not initial
      or s_monat[] is not initial
      or s_gjahr[] is not initial
      or s_aufnr[] is not initial.

        "MESSAGE 'Step 2 - Configurando parametros de seleção...' TYPE 'S'.

        loop at s_aufnr assigning field-symbol(<fs_aufnr>).
          wa_ordens-aufnr = <fs_aufnr>-low.
          append wa_ordens to it_ordens.
          free wa_ordens.
        endloop.

        sort it_ordens.
        delete adjacent duplicates from it_ordens.

        data lv_string type string.
        lv_string = s_prot-low.
        go->set( i_area    = s_kokrs-low
                 i_perio   = s_monat-low
                 i_ano     = s_gjahr-low
                 i_prot    = lv_string
                 it_ordens = it_ordens[] ).
      else.
        if sy-batch is initial.
          message 'Parametro de seleção obrigatório!'(e04) type 'I' display like 'E'.
          exit.
        endif.
      endif.


*      MESSAGE 'Step 3 - Iniciando seleção...' TYPE 'S'.
*      MESSAGE |s_kokrs-low: {  s_kokrs-low } | TYPE 'S'.
*      MESSAGE |s_monat-low: {  s_monat-low } | TYPE 'S'.
*      MESSAGE |s_gjahr-low: {  s_gjahr-low } | TYPE 'S'.
*      MESSAGE |lv_string:   {  lv_string } | TYPE 'S'.
*      LOOP AT it_ordens INTO DATA(lwa_ordem).
*        MESSAGE |LWA_ORDEM:   {  lwa_ordem-aufnr } | TYPE 'S'.
*      ENDLOOP.

      go->run( importing lt_message = lv_message
                         it_conf    = it_conf
                        ).

*      IF it_conf[] IS NOT INITIAL.
** ALV Class
*        cl_salv_table=>factory(
*          IMPORTING
*            r_salv_table = go_alv
*          CHANGING
*            t_table      = it_conf
*        ).
*      ENDIF.

** Display ALV
*      IF go_alv IS BOUND.
      if it_conf[] is not initial.

        message 'Step 4 - Dados encontrados! Iniciando preenchimento Spool ...' type 'S'.
        data lv_info type string.
        lv_ultimo = lines( it_conf ).

        write: /(1) '['.
        lv_info = '['.
        loop at it_conf assigning field-symbol(<fs_conf>).
          lv_index = lv_index + 1.
          select single maktx into @data(_maktx) from makt where  matnr = @<fs_conf>-produto and spras = 'P'.
          write: /(1) '{',
                  (13) '"chavenfe": "',
                  (44) <fs_conf>-chave_nfe,
                  (2) '",',
                  (12) '"Produto": "',
                  (40) <fs_conf>-produto,
                  (2) '",',
                  (14) '"Descricao": "',
                  (40) _maktx,
                  (2) '",',
                  (15) '"cod_clifor": "',
                  (10) <fs_conf>-cod_clifor,
                  (2) '",',
                  (16) '"nome_clifor": "',
                  (35) <fs_conf>-nome_clifor,
                  (2) '",',
                  (13) '"cpf_prod": "',
                  (16) <fs_conf>-cpf_prod,
                  (2) '",',
                  (11) '"nfenum": "',
                  (12) <fs_conf>-nfenum && '-' && <fs_conf>-series,
                  (2) '",',
*                    (12) '"Produto": "',
*                    (03) <fs_conf>-series,
*                    (2) '",',
                  (11) '"docdat": "',
                  (08) <fs_conf>-docdat,
                  (2) '",',
                  (11) '"netwrt": "',
                  (15) <fs_conf>-netwrt,
                  (2) '",',
                  (14) '"prod_cfop": "',
                  (04) <fs_conf>-prod_cfop,
                  (2) '",',
                  (13) '"icms_cst": "',
                  (02) <fs_conf>-icms_cst,
                  (2) '",',
                  (11) '"pstdat": "',
                  (08) <fs_conf>-pstdat,
                  (2) '",',
                  (10) '"ordem": "',
                  (12) <fs_conf>-ordem,
                  (2) '",',
                  (10) '"Solic": "',
                  (12) <fs_conf>-solicitacao_invest,
                  (2) '"'.

          lv_info =  lv_info && '{' && '"chavenfe": "' && <fs_conf>-chave_nfe && '",'.
          lv_info =  lv_info && '"Produto": "' && <fs_conf>-produto && '",'.
          lv_info =  lv_info && '"Descrição": "' && _maktx && '",'.
          lv_info =  lv_info && '"cod_clifor": "' && <fs_conf>-cod_clifor && '",'.
          lv_info =  lv_info && '"nome_clifor": "' && <fs_conf>-nome_clifor && '",'.
          lv_info =  lv_info && '"cpf_prod": "' && <fs_conf>-cpf_prod && '",'.
          lv_info =  lv_info && '"nfenum": "' && <fs_conf>-nfenum && '-' && <fs_conf>-series && '",'.
          lv_info =  lv_info && '"docdat": "' && <fs_conf>-docdat && '",'.
          lv_info =  lv_info && '"netwrt": "' && <fs_conf>-netwrt && '",'.
          lv_info =  lv_info && '"prod_cfop": "' && <fs_conf>-prod_cfop && '",'.
          lv_info =  lv_info && '"icms_cst": "' && <fs_conf>-icms_cst && '",'.
          lv_info =  lv_info && '"pstdat": "' && <fs_conf>-pstdat && '",'.
          lv_info =  lv_info && '"ordem": "' && <fs_conf>-ordem && '",'.
          lv_info =  lv_info && '"solicitacao_invest": "' && <fs_conf>-solicitacao_invest && '"'.
          if lv_index ne lv_ultimo.
            write (2) '},'.
            lv_info =  lv_info && '},'.
          else.
            write (1) '}'.
            lv_info =  lv_info && '}'.
          endif.
        endloop.
        write: /(1) ']'.
        lv_info =  lv_info && ']'.

        message 'Step 4 - Finalizando preenchimento spool...' type 'S'.



*
*DATA: lr_functions TYPE REF TO cl_salv_functions_list.

*          lr_functions = go_alv->get_functions( ).
*          lr_functions->set_all( 'X' ).

*        IF go_alv IS BOUND.
**    IF i_popup = 'X'.
*          go_alv->set_screen_popup(
*            start_column = 1
*            end_column   = 500
*            start_line   = 1
*            end_line     = 200 ).
**    ENDIF.

*          go_alv->display( ).
*        ENDIF.
      else.
        message 'Step 6 - Nenhum dado encontrado...' type 'S'.

        if sy-batch is initial and lv_message is not initial.
          message lv_message type 'I' display like 'E'.
          exit.
        else.
          if lv_message is initial.
            message 'Nenhum resgistro encontrado para seleção!'(e02) type 'I' display like 'E'.
            exit.
          endif.
        endif.
      endif.
*    CATCH cx_salv_msg.
**            WRITE: / 'ALV error'.
*      IF sy-batch IS INITIAL.
*        MESSAGE 'Erro no processamento!'(e01) TYPE 'I' DISPLAY LIKE 'E'.
*      ENDIF.
    catch cx_root.
      if sy-batch is initial.
        message 'Erro no processamento!'(e01) type 'I' display like 'E'.
      else.
        write: / 'Error no processamento!'.
      endif.
  endtry.

end-of-selection.
