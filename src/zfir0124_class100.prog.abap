
CLASS lcl_report_100 DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_sistetico,
             conta     TYPE zfit0232-hkont,
             descricao TYPE zfit0232-name1,
             vlr_brl   TYPE zfit0232-vlr_brl, "decfloat34,
             vlr_usd   TYPE zfit0232-vlr_usd, "decfloat34,
           END OF ty_sistetico.

    TYPES: BEGIN OF ty_analitico,
             id       TYPE zfit0232-id,      "CONTA
             bukrs    TYPE zfit0232-bukrs,
             ano      TYPE zfit0232-ano,
             mes      TYPE zfit0232-mes,
             abertura TYPE zfit0232-abertura,
             belnr    TYPE zfit0232-belnr,
             hkont    TYPE zfit0232-hkont,      "CONTA
             clifor   TYPE zfit0232-clifor,      "CLIENTE/FORNECEDOR ID
             name1    TYPE zfit0232-name1,       "CLIENTE/FORNECEDOR NM
             partidas TYPE char15,
             ov       TYPE zfit0232-ov,      "ORDEM DE VENDA
             blart    TYPE zfit0232-blart,      "TP. DOC.
             budat    TYPE zfit0232-budat,      "DT. DOCTO
             bldat    TYPE zfit0232-bldat,      "DT. LCTO.
             zfbdt    TYPE zfit0232-zfbdt,      "DT. VENCTO
             gjahr    TYPE zfit0232-gjahr,      "ANO
             gsber    TYPE zfit0232-gsber,      "DIVISÃO
             vlr_brl  TYPE zfit0232-vlr_brl,      "VALOR BRL
             vlr_usd  TYPE zfit0232-vlr_usd,      "VALOR USD
             histo    TYPE zfit0232-histo,    "HISTÓRICO  CAMPO LIVRE PARA EDIÇÃO
             class    TYPE zfit0232-class,      "CLASSIFICAÇÃO  CRIAR UM MATHCODE COM AS OPÇÕES:  1 - CONSTITUIR PCLD  2 - MANTER SALDO
             class2   TYPE dd07t-ddtext,      "CLASSIFICAÇÃO  CRIAR UM MATHCODE COM AS OPÇÕES:  1 - CONSTITUIR PCLD  2 - MANTER SALDO
             status1  TYPE icon-name,    "STATUS
             status_n TYPE icon-name,    "STATUS
             doc1     TYPE zib_contabil_chv-belnr,     "DOCUMENTO
             est1     TYPE bkpf-stblg,    "ESTORNO 1
             status2  TYPE icon-name,    "STATUS
             doc2     TYPE zib_contabil_chv-belnr,    "DOC. REVERSÃO
             est2     TYPE bkpf-stblg,    "ESTORNO 2
             obj_key  TYPE zfit0232-obj_key,  "EST. DOC. REV.
             shkzg    TYPE bseg-shkzg,  "EST. DOC. REV.
           END OF ty_analitico.

    DATA: o_alv01        TYPE REF TO cl_gui_alv_grid,
          o_alv02        TYPE REF TO cl_gui_alv_grid,
          o_alv03        TYPE REF TO cl_gui_alv_grid,
          it_fieldcat_03 TYPE lvc_t_fcat,
          it_fieldcat_02 TYPE lvc_t_fcat,
          it_fieldcat_01 TYPE lvc_t_fcat,
          it_f4_03       TYPE lvc_t_f4,
          it_saida01     TYPE STANDARD TABLE OF ty_sistetico INITIAL SIZE 0,
          it_saida02     TYPE STANDARD TABLE OF ty_sistetico INITIAL SIZE 0,
          it_saida03     TYPE STANDARD TABLE OF ty_analitico INITIAL SIZE 0.



    METHODS:

      generate_output1,
      get_data01,
      set_columns_out1,

      generate_output2,
      get_data02,
      set_columns_out2,

      create_data03,
      get_data03,
      generate_output3,
      set_refresh_out3,
      set_columns_out3,

      contabilizar_out3,
      reprocessar_out3,
      reversao_out3,
      limpar_out3,
      estornar_out3,
      go_fb03          "Sem os dois pontos
        CHANGING
          _bln TYPE belnr_d
          _buk TYPE bukrs
          _gjr TYPE gjahr,
      aguarde.

    METHODS:
      on_f4_out3 FOR EVENT onf4 OF cl_gui_alv_grid IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells,
      on_hotspot3 FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id e_row_id es_row_no,
      on_toolbar_out3 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      on_user_command_out3 FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.
CLASS lcl_report_100 IMPLEMENTATION.

  METHOD on_data_changed. "Alheiros

  ENDMETHOD.

  METHOD data_changed_finished. "Alheiros

    DATA: wa_saida03 TYPE ty_analitico,
          w_stable   TYPE lvc_s_stbl.

    LOOP AT et_good_cells INTO DATA(wa_good_cells).

      READ TABLE me->it_saida03
           ASSIGNING FIELD-SYMBOL(<fs_saida_3>)
           INDEX wa_good_cells-row_id.

      IF sy-subrc IS INITIAL.

      ENDIF.
    ENDLOOP.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    IF et_good_cells[] IS NOT INITIAL.
*      PERFORM f_salvar.
      CALL METHOD o_alv03->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.
  ENDMETHOD.

* ... existing code ...
  METHOD go_fb03.
    SET PARAMETER ID 'BLN' FIELD _bln.
    SET PARAMETER ID 'BUK' FIELD _buk.
    SET PARAMETER ID 'GJR' FIELD _gjr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDMETHOD.

  METHOD aguarde.

    DATA: lv_seconds     TYPE i,
          lv_seconds_chr TYPE char3,  "Tipo caractere para conversão
          lv_output      TYPE string.

    "Loop com DO para contagem regressiva
    DO 60 TIMES.
      "Calcula os segundos restantes
      lv_seconds = 61 - sy-index.

      "Converte número para caractere
      lv_seconds_chr = lv_seconds.
      CONDENSE lv_seconds_chr.

      "Formata a mensagem de saída
      CONCATENATE '>>> Aguarde:'
                lv_seconds_chr
                'segundos <<<'
           INTO lv_output
           SEPARATED BY space.

      "Exibe o popup
      MESSAGE lv_output TYPE 'S' DISPLAY LIKE 'I'.

      "Aguarda 1 segundo
      WAIT UP TO '1' SECONDS.

      "Verifica se o WAIT foi executado corretamente
      IF sy-subrc <> 0.
        MESSAGE 'Erro na execução!' TYPE 'E'.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD limpar_out3.

    LOOP AT me->it_saida03 INTO DATA(w_del) WHERE doc1 IS NOT INITIAL OR doc2 IS NOT INITIAL OR est1 IS NOT INITIAL OR est2 IS NOT INITIAL.
      DATA(_delete) = abap_true.
      EXIT.
    ENDLOOP.

    IF _delete <> abap_true.
      DELETE FROM zfit0232 WHERE ano = p_gjahr-low AND mes = p_monat-low AND bukrs = p_bukrs-low.
      IF sy-subrc = 0.
        COMMIT WORK.
        FREE: me->it_saida01,me->it_saida02,me->it_saida03.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      lo_report_100->set_refresh_out3(  ).
    ELSE.
      MESSAGE 'não é possivel Limpar pois exitem Documentos contabilizados!' TYPE 'I'.
    ENDIF.


  ENDMETHOD.

  METHOD on_hotspot3.

    READ TABLE me->it_saida03 INTO DATA(_row) INDEX e_row_id-index.

    IF sy-subrc = 0.

      CASE e_column_id-fieldname.
        WHEN 'STATUS1'.

          IF _row-status1 = '@S_TL_R@' AND _row-obj_key IS NOT INITIAL.
            SELECT * FROM zib_contabil_err WHERE obj_key = @_row-obj_key INTO TABLE @DATA(it_erro1).
            cl_demo_output=>display( it_erro1 ).
          ENDIF.

        WHEN 'STATUS2'.

          IF _row-status2 = '@S_TL_R@' AND _row-obj_key IS NOT INITIAL.

            DATA: _obj_key2 TYPE zib_contabil-obj_key,
                  _len      TYPE i.
            IF _obj_key2+_len(1) = 'N'.
              _obj_key2+_len(1) = 'R'.

              SELECT * FROM zib_contabil_err WHERE obj_key = @_obj_key2 INTO TABLE @DATA(it_erro2).
              cl_demo_output=>display( it_erro2 ).
            ENDIF.
          ENDIF.

        WHEN 'DOC1'.
          me->go_fb03(
            CHANGING
              _bln = _row-doc1
              _buk = _row-bukrs
*              _gjr = _row-ano
*              _gjr = _row-budat(4)
              _gjr = _row-abertura(4)
          ).
        WHEN 'BELNR'.
          me->go_fb03(
            CHANGING
              _bln = _row-belnr
              _buk = _row-bukrs
*              _gjr = _row-ano
              _gjr = _row-budat(4)
          ).
        WHEN 'EST1'.

          me->go_fb03(
            CHANGING
              _bln = _row-est1
              _buk = _row-bukrs
*              _gjr = _row-ano
**              _gjr = _row-budat(4)
              _gjr = _row-abertura(4)
          ).

        WHEN 'DOC2'.
          me->go_fb03(
            CHANGING
              _bln = _row-doc2
              _buk = _row-bukrs
*              _gjr = _row-ano
*              _gjr = _row-budat(4)
              _gjr = _row-abertura(4)
          ).
        WHEN 'EST2'.
          me->go_fb03(
            CHANGING
              _bln = _row-est2
              _buk = _row-bukrs
*              _gjr = _row-ano
*              _gjr = _row-budat(4)
              _gjr = _row-abertura(4)
          ).

      ENDCASE.

    ENDIF.

  ENDMETHOD.

  METHOD reversao_out3.

    DATA(get_reversao) = me->it_saida03.

    SORT get_reversao BY status1 ASCENDING.
    DELETE get_reversao WHERE obj_key IS INITIAL AND status1 = '@S_TL_G@'."Semáforo verde; avançar; ok

    DELETE ADJACENT DUPLICATES FROM get_reversao COMPARING obj_key.

    DATA: lr_objkey TYPE RANGE OF zib_contabil-obj_key.

    lr_objkey[] = VALUE #( FOR w_01 IN get_reversao ( option = 'EQ' sign = 'I' low = w_01-obj_key ) ).

    DELETE lr_objkey WHERE low IS INITIAL.

    IF lr_objkey[] IS NOT INITIAL.
      SELECT * FROM zib_contabil WHERE obj_key IN @lr_objkey INTO TABLE @DATA(it_zib_contabil).
      IF sy-subrc = 0.

        DATA: _len                TYPE i,
              it_zib_contabil_rev TYPE TABLE OF zib_contabil,
              _gjahr              TYPE zib_contabil-gjahr,
              _day                TYPE zib_contabil-monat,
              _monat              TYPE zib_contabil-monat,
              _budat              TYPE zib_contabil-budat,
              _bldat              TYPE zib_contabil-bldat.

        _gjahr = p_dtsub+0(4).
        _monat = p_dtsub+4(2).
        _day = p_dtsub+6(2).

        LOOP AT it_zib_contabil INTO DATA(wa_zib_contabil).

          _len = ( numofchar( wa_zib_contabil-obj_key ) - 1 ).

          IF wa_zib_contabil-obj_key+_len(1) = 'N'.
            wa_zib_contabil-obj_key+_len(1) = 'R'.
          ENDIF.

          wa_zib_contabil-gjahr = _gjahr.
          wa_zib_contabil-monat = _monat.

          wa_zib_contabil-budat =  _budat   = |{ _day }.{ _monat }.{ _gjahr }|."Ultimo dia do mês do mês/ano parâmetro 01.04.2025
          wa_zib_contabil-bldat =  _bldat   = |{ _day }.{ _monat }.{ _gjahr }|."Ultimo dia do mês do mês/ano parâmetro 01.04.2025

          IF wa_zib_contabil-bschl = '40'.
            wa_zib_contabil-bschl = '50'.
          ELSEIF wa_zib_contabil-bschl = '50'.
            wa_zib_contabil-bschl = '40'.
          ENDIF.

          wa_zib_contabil-rg_atualizado = 'N'.
          APPEND wa_zib_contabil TO it_zib_contabil_rev.
          CLEAR: wa_zib_contabil,_len.
        ENDLOOP.

        IF it_zib_contabil_rev IS NOT INITIAL.
          INSERT zib_contabil FROM TABLE it_zib_contabil_rev.
          IF sy-subrc = 0.
            COMMIT WORK.
            lo_report_100->set_refresh_out3(  ).
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD estornar_out3.
    DATA(get_estornar) = me->it_saida03.
    SORT get_estornar BY obj_key ASCENDING.
    DELETE get_estornar WHERE obj_key IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM get_estornar COMPARING obj_key.

    DATA: lr_objkey TYPE RANGE OF zib_contabil-obj_key,
          _obj_key2 TYPE zib_contabil-obj_key,
          _len      TYPE i.

    DATA: it_dta TYPE TABLE OF bdcdata,
          wa_dta TYPE bdcdata,
          wg_bdc TYPE bdcdata,
          tg_bdc TYPE TABLE OF bdcdata,
          tg_msg TYPE TABLE OF bdcmsgcoll,
          wg_msg TYPE bdcmsgcoll,
          opt    TYPE ctu_params.

    LOOP AT get_estornar INTO DATA(_estornar) WHERE obj_key IS NOT INITIAL GROUP BY ( hkont = _estornar-hkont ).
      APPEND VALUE #( option = 'EQ' sign = 'I' low = _estornar-obj_key ) TO lr_objkey[].
      _obj_key2 = _estornar-obj_key.
      _len = ( numofchar( _obj_key2 ) - 1 ).
      IF _obj_key2+_len(1) = 'N'.
        _obj_key2+_len(1) = 'R'.
        APPEND VALUE #( option = 'EQ' sign = 'I' low = _obj_key2 ) TO lr_objkey[].
      ENDIF.
    ENDLOOP.

    IF lr_objkey IS NOT INITIAL.

      SELECT * FROM zib_contabil_chv WHERE obj_key IN @lr_objkey[] INTO TABLE @DATA(it_zib_contabil_chv).

      IF sy-subrc = 0.

        DATA: _bukrs TYPE bukrs,
              _belnr TYPE belnr_d,
              _gjahr TYPE gjahr.

        CLEAR: _bukrs, _belnr, _gjahr.


        LOOP AT it_zib_contabil_chv INTO DATA(ls_zib_contabil_chv).

          _bukrs = p_bukrs-low.
          _belnr = ls_zib_contabil_chv-belnr.
          _gjahr = p_gjahr-low.


*          CALL FUNCTION 'CALL_FB08'
*            EXPORTING
*              i_bukrs      = _bukrs
*              i_belnr      = _belnr
*              i_gjahr      = _gjahr
*              i_stgrd      = '01'
*            EXCEPTIONS
*              not_possible = 1
*              OTHERS       = 2.

          CLEAR: it_bdcdata.

          PERFORM z_preenche_bdc USING:
          'SAPMF05A' '0105' 'X'  ' '           ' ',
          ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
          ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
          ' '        ' '    ' '  'RF05A-BELNS' _belnr,
          ' '        ' '    ' '  'BKPF-BUKRS'  _bukrs,
          ' '        ' '    ' '  'RF05A-GJAHS' _gjahr,
          ' '        ' '    ' '  'UF05A-STGRD' '01'.

          opt-dismode = 'E'.
          CALL TRANSACTION 'FB08' USING it_bdcdata OPTIONS FROM opt.
          IF sy-subrc = 0.
            COMMIT WORK.
            WAIT UP TO 4 SECONDS.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
          CLEAR:ls_zib_contabil_chv,_bukrs, _belnr, _gjahr.
        ENDLOOP.
        lo_report_100->set_refresh_out3(  ).
      ENDIF.


    ENDIF.

  ENDMETHOD.

  METHOD reprocessar_out3.
    DATA(get_reprocessar) = me->it_saida03.
    SORT get_reprocessar BY status1 ASCENDING.
    DELETE get_reprocessar WHERE status1 <> '@S_TL_R@'.

    DELETE ADJACENT DUPLICATES FROM get_reprocessar COMPARING obj_key.

    DATA: lr_objkey TYPE RANGE OF zib_contabil-obj_key.

    lr_objkey[] = VALUE #( FOR w_01 IN get_reprocessar ( option = 'EQ' sign = 'I' low = w_01-obj_key ) ).

    IF lr_objkey IS NOT INITIAL.

      UPDATE zib_contabil SET rg_atualizado = 'N'
      WHERE obj_key IN lr_objkey[].
      IF sy-subrc = 0.
        COMMIT WORK.

        DELETE FROM zib_contabil_err WHERE obj_key IN lr_objkey[].
        IF sy-subrc = 0.
          COMMIT WORK.
          lo_report_100->set_refresh_out3(  ).

        ENDIF.

      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD contabilizar_out3.

    DATA(get_contabilizar) = me->it_saida03.

    SORT get_contabilizar BY class ASCENDING.

    DELETE get_contabilizar WHERE class <> '1'.

    DELETE get_contabilizar WHERE ( doc1 IS INITIAL AND est1 IS NOT INITIAL ) OR ( doc1 IS NOT INITIAL AND est1 IS INITIAL ).

    IF get_contabilizar IS NOT INITIAL.

      SORT get_contabilizar BY hkont ASCENDING.

*      DATA: _vlr_brl        TYPE decfloat34,
*            _vlr_usd        TYPE decfloat34,
      DATA: _vlr_brl        TYPE zfit0232-vlr_brl,
            _vlr_usd        TYPE zfit0232-vlr_brl,
            it_zib_contabil TYPE TABLE OF zib_contabil,
            wa_zib_contabil TYPE zib_contabil.

      FREE: it_zib_contabil.

      CLEAR: wa_zib_contabil,_vlr_brl,_vlr_usd.

      DATA: _seq TYPE i.
      DATA: _matriz TYPE gsber.
      DATA: _objkey TYPE zib_contabil-obj_key.
      DATA: _sequencia TYPE num6.
      DATA: _programa TYPE string.
      DATA: _ano TYPE gjahr.
      _programa = sy-cprog.
      _ano = p_gjahr-low.
      _matriz = |{ p_bukrs-low+2(2) }01|.

      CLEAR: _sequencia.

      LOOP AT get_contabilizar INTO DATA(group_contab) GROUP BY ( hkont = group_contab-hkont ).

        LOOP AT get_contabilizar INTO DATA(sum_dados01) WHERE hkont = group_contab-hkont.
          _vlr_brl = _vlr_brl + CONV decfloat34( sum_dados01-vlr_brl ).
          _vlr_usd = _vlr_usd + CONV decfloat34( sum_dados01-vlr_usd ).
        ENDLOOP.
        IF _sequencia IS INITIAL.

          SELECT SINGLE last_seq FROM zi_zfir0124_maxseq_zibcontabil(  p_ano = @_ano ,p_progr = @sy-cprog ) INTO @DATA(lv_last_seq).

          IF sy-subrc <> 0 OR lv_last_seq IS INITIAL.
            _sequencia = _sequencia + 1.
            _objkey = |{ _programa }{ _sequencia }{ CONV string( p_gjahr-low ) }N|. "Se não existir é 1
          ELSE.                                                       "Se existir ele gera sequencial seguinte!
            _sequencia = lv_last_seq.
            _sequencia = _sequencia + 1.
            _objkey = |{ _programa }{ _sequencia }{ p_gjahr-low }N|.
          ENDIF.

        ELSE.

          _sequencia = _sequencia + 1.
          _objkey = |{ _programa }{ _sequencia }{ p_gjahr-low }N|.

        ENDIF.

        DO 2 TIMES.
          _seq = sy-index.

          wa_zib_contabil-obj_key = _objkey.
          wa_zib_contabil-seqitem = |{ _seq PAD = '0' WIDTH = 6 ALIGN = RIGHT }|."Sequencia
          wa_zib_contabil-gsber = _matriz. "Divisão/Filial
          wa_zib_contabil-bukrs = group_contab-bukrs. "Empresa
          wa_zib_contabil-bldat = |{ p_dtfim+6(2) }.{ p_dtfim+4(2) }.{ p_dtfim+0(4) }|."Ultimo dia do mês do mês/ano parâmetro 31.03.2025
          wa_zib_contabil-budat = |{ p_dtfim+6(2) }.{ p_dtfim+4(2) }.{ p_dtfim+0(4) }|."Ultimo dia do mês do mês/ano parâmetro 31.03.2025
          wa_zib_contabil-gjahr = p_gjahr-low."Ano parâmetro
          wa_zib_contabil-monat = p_monat-low."Mês parâmetro
          wa_zib_contabil-blart = 'LM'."FIXO
          wa_zib_contabil-xblnr = 'PROVISÃO PDD'."FIXO
          wa_zib_contabil-wrbtr = _vlr_brl."Valor BRL somado por Conta ALV
          wa_zib_contabil-waers = 'BRL'."FIXO
          wa_zib_contabil-bupla = _matriz. "Divisão/Filial
          wa_zib_contabil-zuonr = 'PROVISÃO PDD'."FIXO
          wa_zib_contabil-waers_i = 'BRL'."FIXO
          wa_zib_contabil-dmbtr = _vlr_brl."Valor BRL somado por Conta ALV
          wa_zib_contabil-waers_f = 'USD'."FIXO
          wa_zib_contabil-dmbe2 = _vlr_usd."Valor USD somado por Conta ALV
          wa_zib_contabil-rg_atualizado = 'N'. "FIXO

          DATA(l_hkont) = group_contab-hkont.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_hkont
            IMPORTING
              output = l_hkont.

          SELECT SINGLE * FROM zfit0231 WHERE hkont_ativo = @l_hkont INTO @DATA(ls_zfit0231).

          CASE _seq.
            WHEN 1.
              wa_zib_contabil-bschl = '50'.
              wa_zib_contabil-hkont = ls_zfit0231-hkont_provisao.
              wa_zib_contabil-sgtxt = ls_zfit0231-sgtxt1.
            WHEN 2.
              wa_zib_contabil-bschl = '40'.
              wa_zib_contabil-hkont = ls_zfit0231-hkont_despesa.
              wa_zib_contabil-sgtxt = ls_zfit0231-sgtxt2.
          ENDCASE.


          APPEND wa_zib_contabil TO it_zib_contabil.
          " Atualizar o obj_key para todas as linhas do grupo
          LOOP AT GROUP group_contab ASSIGNING FIELD-SYMBOL(<fs_group_contab>).
            <fs_group_contab>-obj_key = _objkey.
          ENDLOOP.
          CLEAR: wa_zib_contabil.
        ENDDO.
        CLEAR: wa_zib_contabil,group_contab,_vlr_brl,_vlr_usd.
      ENDLOOP.

      IF it_zib_contabil IS NOT INITIAL.
        INSERT zib_contabil FROM TABLE it_zib_contabil.
        IF sy-subrc = 0.
          COMMIT WORK.

          LOOP AT get_contabilizar ASSIGNING FIELD-SYMBOL(<fs_cont>).
            UPDATE zfit0232 SET obj_key = @<fs_cont>-obj_key WHERE id = @<fs_cont>-id.
            IF sy-subrc = 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.
          ENDLOOP.

        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

      FREE: get_contabilizar.

      "Mensagem final em popup
      MESSAGE 'Contabilização Finalizada, Aguarde o processamento de 1 Minuto!' TYPE 'I'.

      lo_report_100->set_refresh_out3(  ).

    ELSE.

      MESSAGE 'Não é possivel Gerar Contabilização!' TYPE 'I'.

    ENDIF.

  ENDMETHOD.

  METHOD get_data01.

    FREE: me->it_saida01.

    DATA: wa_saida01 TYPE me->ty_sistetico.

    DATA(get_dados01) = me->it_saida03.

    SORT get_dados01 BY class ASCENDING.

    DELETE get_dados01 WHERE class <> '1'.

    IF get_dados01 IS NOT INITIAL.

      SORT get_dados01 BY hkont ASCENDING.

      CLEAR: wa_saida01.

*      DATA: _vlr_brl TYPE decfloat34,
*            _vlr_usd TYPE decfloat34.

      DATA: _vlr_brl TYPE zfit0232-vlr_brl,
            _vlr_usd TYPE zfit0232-vlr_brl.

      CLEAR: wa_saida01,_vlr_brl,_vlr_usd.

      LOOP AT get_dados01 INTO DATA(group_dados01) GROUP BY ( hkont = group_dados01-hkont ).
        wa_saida01-conta = group_dados01-hkont.
        SELECT SINGLE txt50 FROM skat WHERE saknr = @group_dados01-hkont AND spras = 'P' AND ktopl = '0050' INTO @wa_saida01-descricao.
        LOOP AT get_dados01 INTO DATA(sum_dados01) WHERE hkont = group_dados01-hkont.
*          _vlr_brl = _vlr_brl + CONV decfloat34( sum_dados01-vlr_brl ).
*          _vlr_usd = _vlr_usd + CONV decfloat34( sum_dados01-vlr_usd ).

          _vlr_brl = _vlr_brl + sum_dados01-vlr_brl.
          _vlr_usd = _vlr_usd + sum_dados01-vlr_usd.
        ENDLOOP.
        wa_saida01-vlr_brl = _vlr_brl.
        wa_saida01-vlr_usd = _vlr_usd.
        APPEND wa_saida01 TO me->it_saida01.
        CLEAR: wa_saida01,_vlr_brl,_vlr_usd.
      ENDLOOP.

      FREE: get_dados01.

    ENDIF.
  ENDMETHOD.

  METHOD get_data02.

    FREE: me->it_saida02.

    DATA: wa_saida02 TYPE me->ty_sistetico.

    DATA(get_dados02) = me->it_saida03.

    SORT get_dados02 BY class ASCENDING.

    DELETE get_dados02 WHERE class <> '2'.

    IF get_dados02 IS NOT INITIAL.

      SORT get_dados02 BY hkont ASCENDING.

      CLEAR: wa_saida02.

*      DATA: _vlr_brl TYPE decfloat34,
*            _vlr_usd TYPE decfloat34.

      DATA: _vlr_brl TYPE zfit0232-vlr_brl,
            _vlr_usd TYPE zfit0232-vlr_brl.


      CLEAR: wa_saida02,_vlr_brl,_vlr_usd.

      LOOP AT get_dados02 INTO DATA(group_dados02) GROUP BY ( hkont = group_dados02-hkont ).
        wa_saida02-conta = group_dados02-hkont.
        SELECT SINGLE txt50 FROM skat WHERE saknr = @group_dados02-hkont AND spras = 'P' AND ktopl = '0050' INTO @wa_saida02-descricao.
        LOOP AT get_dados02 INTO DATA(sum_dados02) WHERE hkont = group_dados02-hkont.
*          _vlr_brl = _vlr_brl + CONV decfloat34( sum_dados02-vlr_brl ).
          _vlr_brl = _vlr_brl + sum_dados02-vlr_brl.
          _vlr_usd = _vlr_usd + sum_dados02-vlr_usd .
*          _vlr_usd = _vlr_usd + CONV decfloat34( sum_dados02-vlr_usd ).
        ENDLOOP.
        wa_saida02-vlr_brl = _vlr_brl.
        wa_saida02-vlr_usd = _vlr_usd.
        APPEND wa_saida02 TO me->it_saida02.
        CLEAR: wa_saida02,_vlr_brl,_vlr_usd.
      ENDLOOP.

      FREE: get_dados02.

    ENDIF.
  ENDMETHOD.

  METHOD set_refresh_out3.

    me->get_data03( ).

    DATA: ls_stable TYPE lvc_s_stbl.

    ls_stable-row = abap_true.  "Mantém a ordem das linhas
    ls_stable-col = abap_true.  "Mantém a ordem das colunas

    me->o_alv03->refresh_table_display(
      EXPORTING
        is_stable = ls_stable
    ).

    me->get_data01( ).
    me->o_alv01->refresh_table_display( ).

    me->get_data02( ).
    me->o_alv02->refresh_table_display( ).

    SORT me->it_saida03 BY class DESCENDING hkont ASCENDING.

  ENDMETHOD.



  METHOD set_columns_out1.

    DATA: lo_structdescr   TYPE REF TO cl_abap_structdescr,
          lt_components    TYPE cl_abap_structdescr=>component_table,
          ls_component     TYPE cl_abap_structdescr=>component,
          gt_fieldcat      TYPE lvc_t_fcat,
          gs_fieldcat      TYPE lvc_s_fcat,
          lo_dynamic_table TYPE REF TO data,
          lo_dynamic_line  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_structure> TYPE STANDARD TABLE,
                   <ls_table_structure> TYPE any.

    " Criar tabela dinâmica baseada em IT_SAIDA
    CREATE DATA lo_dynamic_table TYPE TABLE OF ty_sistetico.
    ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

    CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
    ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

    " Obter descrição da estrutura de linha
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <ls_table_structure> ).

    lt_components = lo_structdescr->get_components( ).

    IF sy-subrc = 0.

      TYPES: BEGIN OF ty_make_cols,
               fieldname  TYPE lvc_s_fcat-fieldname,
               coltext    TYPE lvc_s_fcat-coltext,
               edit       TYPE lvc_s_fcat-edit,
               outputlen  TYPE lvc_s_fcat-outputlen,
               f4availabl TYPE lvc_s_fcat-f4availabl,
               no_out     TYPE lvc_s_fcat-no_out,
               do_sum     TYPE lvc_s_fcat-do_sum,
             END OF ty_make_cols.

      DATA: it_make_cols TYPE STANDARD TABLE OF ty_make_cols WITH EMPTY KEY.

      "Basta colocar na sequencia que o key será o index da coluna, não existe a necessidade de colocar a sequencia já que é a ordem!
      it_make_cols = VALUE #(
        ( fieldname =   'CONTA'         coltext = 'Conta'         edit = abap_false   outputlen = 10  f4availabl = abap_false  no_out = abap_false  do_sum = abap_false )
        ( fieldname =   'DESCRICAO'     coltext = 'Descrição'     edit = abap_false   outputlen = 20  f4availabl = abap_false  no_out = abap_false  do_sum = abap_false )
        ( fieldname =   'VLR_BRL'       coltext = 'Valor BRL'     edit = abap_false   outputlen = 18  f4availabl = abap_false  no_out = abap_false  do_sum = abap_true  )
        ( fieldname =   'VLR_USD'       coltext = 'Valor USD'     edit = abap_false   outputlen = 18  f4availabl = abap_false  no_out = abap_false  do_sum = abap_true  )
                        ).

      CLEAR: gs_fieldcat.

      FREE: gt_fieldcat,it_fieldcat_01.

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>) WHERE name <> 'MANDT'.

        READ TABLE it_make_cols WITH KEY fieldname = <fs_components>-name INTO DATA(wa_make_cols). "table_line

        IF sy-subrc = 0.

          MOVE-CORRESPONDING wa_make_cols TO gs_fieldcat.
          gs_fieldcat-col_pos = sy-tabix.

        ENDIF.

        APPEND gs_fieldcat TO gt_fieldcat.
        CLEAR: gs_fieldcat.

      ENDLOOP.

      MOVE-CORRESPONDING gt_fieldcat TO it_fieldcat_01.

      FREE: gt_fieldcat.

    ENDIF.

  ENDMETHOD.

  METHOD set_columns_out2.

    DATA: lo_structdescr   TYPE REF TO cl_abap_structdescr,
          lt_components    TYPE cl_abap_structdescr=>component_table,
          ls_component     TYPE cl_abap_structdescr=>component,
          gt_fieldcat      TYPE lvc_t_fcat,
          gs_fieldcat      TYPE lvc_s_fcat,
          lo_dynamic_table TYPE REF TO data,
          lo_dynamic_line  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table_structure> TYPE STANDARD TABLE,
                   <ls_table_structure> TYPE any.

    " Criar tabela dinâmica baseada em IT_SAIDA
    CREATE DATA lo_dynamic_table TYPE TABLE OF ty_sistetico.
    ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

    CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
    ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

    " Obter descrição da estrutura de linha
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <ls_table_structure> ).

    lt_components = lo_structdescr->get_components( ).

    IF sy-subrc = 0.

      TYPES: BEGIN OF ty_make_cols,
               fieldname  TYPE lvc_s_fcat-fieldname,
               coltext    TYPE lvc_s_fcat-coltext,
               edit       TYPE lvc_s_fcat-edit,
               outputlen  TYPE lvc_s_fcat-outputlen,
               f4availabl TYPE lvc_s_fcat-f4availabl,
               no_out     TYPE lvc_s_fcat-no_out,
               do_sum     TYPE lvc_s_fcat-do_sum,
             END OF ty_make_cols.

      DATA: it_make_cols TYPE STANDARD TABLE OF ty_make_cols WITH EMPTY KEY.

      "Basta colocar na sequencia que o key será o index da coluna, não existe a necessidade de colocar a sequencia já que é a ordem!
      it_make_cols = VALUE #(
        ( fieldname =   'CONTA'         coltext = 'Conta'         edit = abap_false   outputlen = 10  f4availabl = abap_false  no_out = abap_false do_sum = abap_false )
        ( fieldname =   'DESCRICAO'     coltext = 'Descrição'     edit = abap_false   outputlen = 20  f4availabl = abap_false  no_out = abap_false do_sum = abap_false )
        ( fieldname =   'VLR_BRL'       coltext = 'Valor BRL'     edit = abap_false   outputlen = 18  f4availabl = abap_false  no_out = abap_false do_sum = abap_true  )
        ( fieldname =   'VLR_USD'       coltext = 'Valor USD'     edit = abap_false   outputlen = 18  f4availabl = abap_false  no_out = abap_false do_sum = abap_true  )
                        ).

      CLEAR: gs_fieldcat.

      FREE: gt_fieldcat,it_fieldcat_02.

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>) WHERE name <> 'MANDT'.

        READ TABLE it_make_cols WITH KEY fieldname = <fs_components>-name INTO DATA(wa_make_cols). "table_line

        IF sy-subrc = 0.

          MOVE-CORRESPONDING wa_make_cols TO gs_fieldcat.
          gs_fieldcat-col_pos = sy-tabix.

        ENDIF.

        APPEND gs_fieldcat TO gt_fieldcat.
        CLEAR: gs_fieldcat.

      ENDLOOP.

      MOVE-CORRESPONDING gt_fieldcat TO it_fieldcat_02.

      FREE: gt_fieldcat.

    ENDIF.

  ENDMETHOD.

  METHOD set_columns_out3 .


    DATA: lo_structdescr   TYPE REF TO cl_abap_structdescr,
          lt_components    TYPE cl_abap_structdescr=>component_table,
          ls_component     TYPE cl_abap_structdescr=>component,
          gt_fieldcat      TYPE lvc_t_fcat,
          gs_fieldcat      TYPE lvc_s_fcat,
          lo_dynamic_table TYPE REF TO data,
          lo_dynamic_line  TYPE REF TO data,
          lt_f4            TYPE STANDARD TABLE OF lvc_s_f4 INITIAL SIZE 0,
          ls_f4            TYPE lvc_s_f4.

    FIELD-SYMBOLS: <lt_table_structure> TYPE STANDARD TABLE,
                   <ls_table_structure> TYPE any.

    " Criar tabela dinâmica baseada em IT_SAIDA
    CREATE DATA lo_dynamic_table TYPE TABLE OF ty_analitico.
    ASSIGN lo_dynamic_table->* TO <lt_table_structure>.

    CREATE DATA lo_dynamic_line LIKE LINE OF <lt_table_structure>.
    ASSIGN lo_dynamic_line->* TO <ls_table_structure>.

    " Obter descrição da estrutura de linha
    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( <ls_table_structure> ).

    lt_components = lo_structdescr->get_components( ).

    IF sy-subrc = 0.

      TYPES: BEGIN OF ty_make_cols,
               fieldname  TYPE lvc_s_fcat-fieldname,
               coltext    TYPE lvc_s_fcat-coltext,
               edit       TYPE lvc_s_fcat-edit,
               outputlen  TYPE lvc_s_fcat-outputlen,
               f4availabl TYPE lvc_s_fcat-f4availabl,
               no_out     TYPE lvc_s_fcat-no_out,
               hotspot    TYPE lvc_s_fcat-hotspot,    "Adicionado campo hotspot
               just       TYPE lvc_s_fcat-just,
             END OF ty_make_cols.

      DATA: it_make_cols TYPE STANDARD TABLE OF ty_make_cols WITH EMPTY KEY.

      "Basta colocar na sequencia que o key será o index da coluna, não existe a necessidade de colocar a sequencia já que é a ordem!
      it_make_cols = VALUE #(
        ( fieldname =   'BELNR'     coltext = 'Doc.Origem'            edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'L')
        ( fieldname =   'HKONT'     coltext = 'Conta'                 edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'CLIFOR'    coltext = 'Cliente/Fornecedor Id' edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'NAME1'     coltext = 'Cliente/Fornecedor Nm' edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'OV'        coltext = 'Ordem De Venda'        edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'PARTIDAS'  coltext = 'Partidas'              edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'BLART'     coltext = 'Tp. Doc'               edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'BUDAT'     coltext = 'Dt. Docto'             edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'BLDAT'     coltext = 'Dt. Lcto.'             edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'ZFBDT'     coltext = 'Dt. Vencto'            edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'GJAHR'     coltext = 'Ano Vencimento'       edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'GSBER'     coltext = 'Divisão'               edit = abap_false   outputlen = 10  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'VLR_BRL'   coltext = 'Valor Brl'             edit = abap_false   outputlen = 18  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'VLR_USD'   coltext = 'Valor Usd'             edit = abap_false   outputlen = 18  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'HISTO'     coltext = 'Histórico'             edit = abap_true    outputlen = 50  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'CLASS'     coltext = 'Class.Cod'             edit = abap_true    outputlen = 10  f4availabl = abap_true    no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'CLASS2'    coltext = 'Classificação'         edit = abap_false   outputlen = 14  f4availabl = abap_false   no_out = abap_false hotspot = abap_false  just = 'L')
        ( fieldname =   'STATUS1'   coltext = 'Contabilizado'         edit = abap_false   outputlen = 14  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
        ( fieldname =   'STATUS_N'  coltext = 'Contabilizado Status'  edit = abap_false   outputlen = 14  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
        ( fieldname =   'DOC1'      coltext = 'Doc. Contabil'         edit = abap_false   outputlen = 14  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
        ( fieldname =   'EST1'      coltext = 'Est.Doc. Contabil'     edit = abap_false   outputlen = 20  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
        ( fieldname =   'DOC2'      coltext = 'Doc. Reversão'         edit = abap_false   outputlen = 14  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
        ( fieldname =   'EST2'      coltext = 'Est.Doc. Reversão'     edit = abap_false   outputlen = 20  f4availabl = abap_false   no_out = abap_false hotspot = abap_true   just = 'C')
                        ).

      CLEAR: gs_fieldcat,ls_f4.

      FREE: gt_fieldcat,it_fieldcat_03,lt_f4.
      DATA: lr_fieldname TYPE RANGE OF abap_compname.
      "EXCLUI AS LINHAS DE GERAR O FIELDCATALOG
      lr_fieldname = VALUE #(
       ( option = 'EQ' sign = 'I' low = 'MANDT' )
       ( option = 'EQ' sign = 'I' low = 'ID' )
       ( option = 'EQ' sign = 'I' low = 'BUKRS' )
       ( option = 'EQ' sign = 'I' low = 'ANO' )
       ( option = 'EQ' sign = 'I' low = 'MES' )
       ( option = 'EQ' sign = 'I' low = 'ABERTURA' )
       ( option = 'EQ' sign = 'I' low = 'OBJ_KEY' )
       ( option = 'EQ' sign = 'I' low = abap_false )
      ).

      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<fs_components>) WHERE name NOT IN lr_fieldname.

        READ TABLE it_make_cols WITH KEY fieldname = <fs_components>-name INTO DATA(wa_make_cols). "table_line

        IF sy-subrc = 0.

          MOVE-CORRESPONDING wa_make_cols TO gs_fieldcat.
          gs_fieldcat-col_pos = sy-tabix.

          IF  wa_make_cols-f4availabl = abap_true.
            ls_f4-register = abap_true.
            ls_f4-fieldname  = <fs_components>-name.
            APPEND ls_f4 TO lt_f4.
            CLEAR:ls_f4.
          ENDIF.

        ENDIF.

        APPEND gs_fieldcat TO gt_fieldcat.
        CLEAR: gs_fieldcat.

      ENDLOOP.

      DELETE gt_fieldcat WHERE fieldname = abap_false.
      MOVE-CORRESPONDING gt_fieldcat TO it_fieldcat_03.
      MOVE-CORRESPONDING lt_f4 TO it_f4_03.

      FREE: gt_fieldcat, lt_f4.

    ENDIF.


  ENDMETHOD.

  METHOD get_data03.
    FREE: me->it_saida03.

    DATA: lv_lifnr TYPE bseg-lifnr.

    SELECT * FROM zfit0232
     WHERE ano = @p_gjahr-low
      AND mes = @p_monat-low
      AND bukrs = @p_bukrs-low
      AND hkont IN @p_hkont
      INTO CORRESPONDING FIELDS OF TABLE @me->it_saida03.

    SELECT * FROM zi_class_f4_zfit0232 INTO TABLE @DATA(it_class).

    IF me->it_saida03 IS NOT INITIAL.

      DATA: _obj_key2 TYPE zib_contabil-obj_key,
            _len      TYPE i.

      IF it_sap IS INITIAL.
        SELECT DISTINCT *
          FROM zfit0231
          INTO TABLE it_sap
          WHERE hkont_ativo IN p_hkont.
      ENDIF.

      LOOP AT me->it_saida03 ASSIGNING FIELD-SYMBOL(<fs_saida>).

        IF p_hkont IS NOT INITIAL.
          READ TABLE it_sap INTO DATA(ls_it_sap)
               WITH KEY hkont_ativo = <fs_saida>-hkont.

          IF sy-subrc IS NOT INITIAL .
            CLEAR <fs_saida>-hkont.
          ENDIF.
        ENDIF.

        <fs_saida>-ano = <fs_saida>-zfbdt(4).
        <fs_saida>-gjahr = <fs_saida>-zfbdt(4).

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            input  = <fs_saida>-hkont
*          IMPORTING
*            output = <fs_saida>-hkont.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_saida>-clifor
          IMPORTING
            output = <fs_saida>-clifor.

        IF <fs_saida>-class IS NOT INITIAL. "Preeenche classificação
          READ TABLE it_class ASSIGNING FIELD-SYMBOL(<fs_class>) WITH KEY domvalue_l = <fs_saida>-class.
          IF sy-subrc = 0.
            <fs_saida>-class2 = <fs_class>-ddtext.
          ENDIF.
        ENDIF.

        IF <fs_saida>-status1 IS INITIAL.

          IF <fs_saida>-obj_key IS NOT INITIAL.

            SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<fs_saida>-obj_key INTO @DATA(ls_zib_contabil_err1).
            IF sy-subrc = 0.
              <fs_saida>-status1 = '@S_TL_R@'."Semáf.verm.; parar; incorreto
              <fs_saida>-status_n = |Erro contabilização|.
              CLEAR: _obj_key2,_len.
              _obj_key2 = <fs_saida>-obj_key.
              _len = ( numofchar( _obj_key2 ) - 1 ).

              IF _obj_key2+_len(1) = 'N'.
                _obj_key2+_len(1) = 'R'.

                SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @_obj_key2 INTO @DATA(ls_zib_contabil_err2).
                IF sy-subrc = 0.
                  <fs_saida>-status2 =  '@S_TL_R@'."Semáf.verm.; parar; incorreto
                  <fs_saida>-status_n = |Erro contabilização|.
                ENDIF.

              ENDIF.

            ENDIF.

            SELECT SINGLE * FROM zib_contabil_chv WHERE obj_key = @<fs_saida>-obj_key INTO @DATA(ls_zib_contabil_chv1).
            IF sy-subrc = 0.
              <fs_saida>-status1 = '@S_TL_G@'."Semáforo verde; avançar; ok
              <fs_saida>-status_n = |Contabilizado |.
              <fs_saida>-doc1 = ls_zib_contabil_chv1-belnr.
              CLEAR: _obj_key2,_len.
              _obj_key2 = <fs_saida>-obj_key.
              _len = ( numofchar( _obj_key2 ) - 1 ).

              SELECT SINGLE stblg FROM bkpf WHERE bukrs = @p_bukrs-low AND belnr = @ls_zib_contabil_chv1-belnr AND gjahr = @p_gjahr-low INTO @<fs_saida>-est1.

              IF _obj_key2+_len(1) = 'N'.
                _obj_key2+_len(1) = 'R'.

                SELECT SINGLE * FROM zib_contabil_chv WHERE obj_key = @_obj_key2 INTO @DATA(ls_zib_contabil_chv2).
                IF sy-subrc = 0.
                  <fs_saida>-status2 = '@S_TL_G@'."Semáforo verde; avançar; ok
                  <fs_saida>-status_n = |Contabilizado |.
                  <fs_saida>-doc2 = ls_zib_contabil_chv2-belnr.
                  SELECT SINGLE stblg FROM bkpf WHERE bukrs = @p_bukrs-low AND belnr = @ls_zib_contabil_chv2-belnr AND gjahr = @p_gjahr-low INTO @<fs_saida>-est2.
                ENDIF.
              ENDIF.

            ENDIF.

            IF <fs_saida>-status1 IS INITIAL.
              <fs_saida>-status1 = '@S_TL_Y@'."Semáforo amarelo;atenção
              <fs_saida>-status_n = |Disponível para contabilizar|.
            ENDIF.

          ELSE.
            IF <fs_saida>-class = '1'.
              <fs_saida>-status1 = '@S_TL_Y@'."Semáforo amarelo;atenção
              <fs_saida>-status_n = |Disponível para contabilizar|.
            ELSEIF <fs_saida>-class = '2'.
              <fs_saida>-status1 = '@WFWIOL@'. "Nulo
              <fs_saida>-status_n = |Não gera contabilização |.
            ELSE.
              <fs_saida>-status1 = '@OUTLIG@'. "Semáforo apagado; Indefinido
              <fs_saida>-status_n = |Fazer classificação|.
            ENDIF.
          ENDIF.

        ENDIF.

        lv_lifnr = <fs_saida>-clifor.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_lifnr
          IMPORTING
            output = lv_lifnr.

*        SELECT SINGLE shkzg
*          FROM bseg
*          INTO @DATA(ls_shkzg)
*          WHERE bukrs = @<fs_saida>-bukrs
*          AND   gjahr = @<fs_saida>-gjahr
*          AND   belnr = @<fs_saida>-belnr.
*        IF sy-subrc IS INITIAL.

        IF <fs_saida>-shkzg = 'H'.
          <fs_saida>-vlr_brl = <fs_saida>-vlr_brl * -1.
          <fs_saida>-vlr_usd = <fs_saida>-vlr_usd * -1.
        ENDIF.

*        ENDIF.


      ENDLOOP.
    ENDIF.

    DELETE me->it_saida03 WHERE hkont IS INITIAL.

    SORT me->it_saida03 BY class DESCENDING hkont ASCENDING.

  ENDMETHOD.

  METHOD create_data03.

    DATA: wa_saida03      TYPE ty_analitico.

    FREE: me->it_saida03.

    "IF p_bsid = abap_true.
    SELECT *
      FROM zi_zfir0124_cliente_bsid( p_abe_bsid = @p_paber-low , p_emp_bsid = @p_bukrs-low )
      INTO TABLE @DATA(it_dados_cliente_bsid).
    IF sy-subrc = 0.
      LOOP AT it_dados_cliente_bsid ASSIGNING FIELD-SYMBOL(<fs_bsid>).
        MOVE-CORRESPONDING <fs_bsid> TO wa_saida03.
        wa_saida03-partidas = 'Cli - Em aberto'.
        APPEND wa_saida03 TO me->it_saida03.
        CLEAR: wa_saida03.
      ENDLOOP.
      FREE: it_dados_cliente_bsid.
    ENDIF.
    "ENDIF.

    "IF p_bsad  = abap_true.
    SELECT *
      FROM zi_zfir0124_cliente_bsad( p_emp_bsad = @p_bukrs-low, p_abe_bsad = @p_paber-low )
     INTO TABLE @DATA(it_dados_cliente_bsad).
    IF sy-subrc = 0.
      LOOP AT it_dados_cliente_bsad ASSIGNING FIELD-SYMBOL(<fs_bsad>).
        MOVE-CORRESPONDING <fs_bsad> TO wa_saida03.
        wa_saida03-partidas = 'Cli - Baixadas'.
        APPEND wa_saida03 TO me->it_saida03.
        CLEAR: wa_saida03.
      ENDLOOP.
      FREE: it_dados_cliente_bsad.
    ENDIF.
    "ENDIF.

    "IF p_bsik = abap_true.
    SELECT *
      FROM zi_zfir0124_fornecedor_bsik( p_emp_bsik = @p_bukrs-low, p_abe_bsik = @p_paber-low )
      INTO TABLE @DATA(it_dados_fornecedor_bsik).
    IF sy-subrc = 0.
      LOOP AT it_dados_fornecedor_bsik ASSIGNING FIELD-SYMBOL(<fs_bsik>).
        MOVE-CORRESPONDING <fs_bsik> TO wa_saida03.
        wa_saida03-partidas = '	For - Em aberto'.
        APPEND wa_saida03 TO me->it_saida03.
        CLEAR: wa_saida03.
      ENDLOOP.
      FREE: it_dados_fornecedor_bsik.
    ENDIF.
    "ENDIF.

    "IF p_bsak = abap_true.
    SELECT *
       FROM zi_zfir0124_fornecedor_bsak( p_emp_bsak = @p_bukrs-low, p_abe_bsak = @p_paber-low )
      INTO TABLE @DATA(it_dados_fornecedor_bsak).
    IF sy-subrc = 0.
      LOOP AT it_dados_fornecedor_bsak ASSIGNING FIELD-SYMBOL(<fs_bsak>).
        MOVE-CORRESPONDING <fs_bsak> TO wa_saida03.
        wa_saida03-partidas = 'For - Baixadas'.
        APPEND wa_saida03 TO me->it_saida03.
        CLEAR: wa_saida03.
      ENDLOOP.
      FREE: it_dados_fornecedor_bsak.
    ENDIF.
    "ENDIF.

    SORT me->it_saida03 BY class DESCENDING hkont ASCENDING.

    LOOP AT me->it_saida03 ASSIGNING FIELD-SYMBOL(<fs_saida>).
      IF <fs_saida>-shkzg = 'H'.
        <fs_saida>-vlr_brl = <fs_saida>-vlr_brl * -1.
        <fs_saida>-vlr_usd = <fs_saida>-vlr_usd * -1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_user_command_out3.
    DATA: lt_rows     TYPE lvc_t_row,
          ls_row      TYPE lvc_s_roid,
          qtd_rows    TYPE int4,
          wa_saida03  TYPE ty_analitico,
          wa_zfit0232 TYPE zfit0232,
          it_zfit0232 TYPE SORTED TABLE OF zfit0232 WITH UNIQUE KEY id.

    IF e_ucomm <> 'REFRESH_GRID'.
      CALL METHOD me->o_alv03->get_selected_rows IMPORTING et_index_rows = lt_rows.
      qtd_rows = lines( lt_rows ).
    ENDIF.

    FREE: it_zfit0232.
    CLEAR: wa_saida03,wa_zfit0232.

    CASE e_ucomm.

      WHEN 'REVERSAO_GRID'.

        LOOP AT me->it_saida03 INTO DATA(_exiterev) WHERE ( doc2 IS NOT INITIAL ).
          DATA(_exit_rev) = abap_true.
          EXIT.
        ENDLOOP.

        IF _exit_rev = abap_false.
          me->reversao_out3(  ).
        ELSE.
          MESSAGE 'Não Existem dados que possam ser reprocessados de erro!' TYPE 'I'.
        ENDIF.

      WHEN 'LIMPAR_GRID'.

        me->limpar_out3(  ).

      WHEN 'REPROCESSAR_GRID'.

        READ TABLE me->it_saida03 INTO DATA(_exiterr) WITH KEY status1 = '@S_TL_R@'.
        IF sy-subrc = 0.
          me->reprocessar_out3(  ).
        ELSE.
          MESSAGE 'Não Existem dados que possam ser reprocessados de erro!' TYPE 'I'.
        ENDIF.

      WHEN 'CONTABILIZAR_GRID'.

        LOOP AT me->it_saida03 INTO DATA(_exitezib) WHERE ( doc1 IS NOT INITIAL AND est1 IS INITIAL ).
          DATA(_exit_cont) = abap_true.
          EXIT.
        ENDLOOP.

        IF _exit_cont = abap_false.
          me->contabilizar_out3(  ).
        ELSE.
          MESSAGE 'Já existem dados na ZIB_CONTABIL!' TYPE 'I'.
        ENDIF.
      WHEN 'ESTORNAR_GRID'.

        LOOP AT me->it_saida03 INTO DATA(_exitojkey) WHERE obj_key IS NOT INITIAL.
          DATA(_exite_objkey) = abap_true.
          EXIT.
        ENDLOOP.

        IF _exite_objkey = abap_true.
          me->estornar_out3(  ).
        ELSE.
          MESSAGE 'Não Existem dados que possam ser Estornado!' TYPE 'I'.
        ENDIF.

      WHEN 'SAVE_GRID'.

        FREE:it_zfit0232.

        IF me->it_saida03 IS NOT INITIAL.

          IF p_create = abap_true.

            LOOP AT me->it_saida03 ASSIGNING FIELD-SYMBOL(<fs_saida03>).

              CALL FUNCTION 'GUID_CREATE'
                IMPORTING
                  ev_guid_16 = <fs_saida03>-id.

              <fs_saida03>-ano = p_gjahr-low.
              <fs_saida03>-mes = p_monat-low.
              <fs_saida03>-bukrs = p_bukrs-low.
              <fs_saida03>-abertura = p_paber-low.

              IF <fs_saida03>-class <> '1' AND <fs_saida03>-class <> '2' AND <fs_saida03>-class <> abap_false.
                <fs_saida03>-class = abap_false.
              ENDIF.

            ENDLOOP.
            MOVE-CORRESPONDING me->it_saida03 TO it_zfit0232.
            INSERT zfit0232 FROM TABLE it_zfit0232.
            COMMIT WORK.
            IF sy-subrc = 0.
              p_create = abap_false.
            ENDIF.

          ELSE.

            LOOP AT me->it_saida03 ASSIGNING <fs_saida03>.

              IF <fs_saida03>-class <> '1' AND <fs_saida03>-class <> '2' AND <fs_saida03>-class <> abap_false.
                <fs_saida03>-class = abap_false.
              ENDIF.

            ENDLOOP.

            MOVE-CORRESPONDING me->it_saida03 TO it_zfit0232.
            "MODIFY zfit0232 FROM TABLE it_zfit0232.
            UPDATE zfit0232 FROM TABLE it_zfit0232.
            COMMIT WORK.

          ENDIF.

        ELSE.
          MESSAGE 'Não existem dados!' TYPE 'I'.
        ENDIF.
        me->set_refresh_out3( ).

      WHEN 'DELETE_ROW'.

        IF qtd_rows > 0.

          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
            READ TABLE me->it_saida03 ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
            MOVE-CORRESPONDING <_del> TO wa_zfit0232.
            DELETE zfit0232 FROM wa_zfit0232.
            COMMIT WORK.
          ENDLOOP.

          CALL METHOD me->set_refresh_out3( ).

        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

      WHEN 'INSERT_ROW'.
        APPEND wa_saida03 TO me->it_saida03.
        CALL METHOD me->o_alv03->refresh_table_display( ).
      WHEN 'REFRESH_GRID' OR 'REFRESH'.
        CALL METHOD me->set_refresh_out3( ).

      WHEN 'EDITAR_GRID'.

        CASE vg_edit.
          WHEN 0.
            vg_edit = 1.
          WHEN 1.
            vg_edit = 0.
          WHEN OTHERS.
        ENDCASE.
*        vg_edit = 0.

*        vg_edit = COND #( WHEN vg_edit IS INITIAL THEN 0 ELSE 1 ).
*** Método de controle de Edição do ALV
        CALL METHOD me->o_alv03->set_ready_for_input( EXPORTING i_ready_for_input = vg_edit ).
*        me->limpar_out3(  ).
*      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD on_toolbar_out3.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA
      "3 DESABILITA E 0 HABILITA

      CASE <fs_tollbar>-function.
*        WHEN
*          '&LOCAL&INSERT_ROW' OR
*          '&LOCAL&APPEND' OR
*          '&LOCAL&DELETE_ROW' OR
*          '&LOCAL&COPY_ROW'.
*          <fs_tollbar>-butn_type = '3'.
        WHEN OTHERS.
          IF <fs_tollbar>-function EQ '&REFRESH'.
            <fs_tollbar>-function = 'REFRESH_GRID'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
            <fs_tollbar>-function = 'DELETE_ROW'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
            <fs_tollbar>-function = 'INSERT_ROW'.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'REFRESH'.   "fcode
    mt_toolbar-icon = '@42@'.
    mt_toolbar-quickinfo = 'Atualizar'.
    mt_toolbar-text = 'Atualizar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'SAVE_GRID'.   "fcode
    mt_toolbar-icon = '@F_SAVE@'.
    mt_toolbar-quickinfo = 'Salvar Grid'.
    mt_toolbar-text = 'Salvar Grid'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'CONTABILIZAR_GRID'.   "fcode
    mt_toolbar-icon = icon_execute_object.
    mt_toolbar-quickinfo = 'Contabilizar'.
    mt_toolbar-text = 'Contabilizar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'REPROCESSAR_GRID'.   "fcode
    mt_toolbar-icon = icon_led_red.
    mt_toolbar-quickinfo = 'Reprocessar'.
    mt_toolbar-text = 'Reprocessar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'ESTORNAR_GRID'.   "fcode
    mt_toolbar-icon = icon_system_undo.
    mt_toolbar-quickinfo = 'Estornar'.
    mt_toolbar-text = 'Estornar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'REVERSAO_GRID'.   "fcode
    mt_toolbar-icon = icon_operation.
    mt_toolbar-quickinfo = 'Reversão'.
    mt_toolbar-text = 'Reversão'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'LIMPAR_GRID'.   "fcode
    mt_toolbar-icon = icon_erase.
    mt_toolbar-quickinfo = 'Limpar Dados'.
    mt_toolbar-text = 'Limpar Dados'.
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
    mt_toolbar-function = 'EDITAR_GRID'.   "fcode
    mt_toolbar-disabled  = ''.   "fcode
    mt_toolbar-icon = icon_change.
    mt_toolbar-quickinfo = 'Editar'.
    mt_toolbar-text = 'Editar'.
    APPEND mt_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD generate_output1.

    DATA: main_container TYPE REF TO cl_gui_custom_container.

    CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.

    CREATE OBJECT:
    main_container EXPORTING container_name = 'C100_1',
      me->o_alv01 EXPORTING i_parent = main_container.

    me->set_columns_out1( ).

    IF me->it_fieldcat_01 IS NOT INITIAL.

      DATA: ls_variant TYPE disvariant.

      ls_variant-report = sy-repid.
      "VALUE disvariant( report = sy-repid )

      TRY.

          CALL METHOD me->o_alv01->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout
              "i_structure_name = 'TY_SAIDA'
              is_variant      = ls_variant
              i_save          = 'A'
            CHANGING
              it_outtab       = me->it_saida01
              it_fieldcatalog = me->it_fieldcat_01.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
    "Resumo das análises - Constituir PDD
  ENDMETHOD.


  METHOD generate_output2.

    DATA: main_container TYPE REF TO cl_gui_custom_container.

    CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.

    CREATE OBJECT:
    main_container EXPORTING container_name = 'C100_2',
      me->o_alv02 EXPORTING i_parent = main_container.

    me->set_columns_out2( ).

    IF me->it_fieldcat_02 IS NOT INITIAL.

      DATA: ls_variant TYPE disvariant.
      ls_variant-report = sy-repid.
      "VALUE disvariant( report = sy-repid )
      TRY.

          CALL METHOD me->o_alv02->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout
              "i_structure_name = 'TY_SAIDA'
              is_variant      = ls_variant
              i_save          = 'A'
            CHANGING
              it_outtab       = me->it_saida02
              it_fieldcatalog = me->it_fieldcat_02.

        CATCH cx_root.
      ENDTRY.

    ENDIF.
    "|Resumo das análises - Manter Saldo|.


  ENDMETHOD.

  METHOD generate_output3.

    DATA: main_container TYPE REF TO cl_gui_custom_container.

    CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.

    CREATE OBJECT:
    main_container EXPORTING container_name = 'C100_3',
    me->o_alv03 EXPORTING i_parent = main_container.

    me->set_columns_out3( ).

    CALL METHOD me->o_alv03->register_f4_for_fields
      EXPORTING
        it_f4 = it_f4_03.

    SET HANDLER:
            me->on_f4_out3 FOR o_alv03,
            me->on_user_command_out3 FOR o_alv03,
            me->on_hotspot3 FOR o_alv03,
            me->on_toolbar_out3 FOR o_alv03,
            me->on_data_changed FOR o_alv03,
            me->data_changed_finished FOR o_alv03.


    IF p_hkont IS NOT INITIAL.
      IF it_sap IS INITIAL.
        SELECT DISTINCT *
          FROM zfit0231
          INTO TABLE it_sap
          WHERE hkont_ativo IN p_hkont.
      ENDIF.
    ENDIF.

    IF me->it_fieldcat_03 IS NOT INITIAL.

      DATA: ls_variant TYPE disvariant.
      ls_variant-report = sy-repid.
      "VALUE disvariant( report = sy-repid )

      LOOP AT me->it_saida03 ASSIGNING FIELD-SYMBOL(<fs_03>).

        IF p_hkont IS NOT INITIAL.
          READ TABLE it_sap INTO DATA(ls_it_sap)
               WITH KEY hkont_ativo = <fs_03>-hkont.

          IF sy-subrc IS NOT INITIAL .
            CLEAR <fs_03>-hkont.
          ENDIF.
        ENDIF.

        <fs_03>-gjahr = <fs_03>-zfbdt(4).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_03>-hkont
          IMPORTING
            output = <fs_03>-hkont.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = <fs_03>-clifor
          IMPORTING
            output = <fs_03>-clifor.
      ENDLOOP.

      IF p_hkont IS NOT INITIAL.
        DELETE me->it_saida03 WHERE hkont IS INITIAL.
      ENDIF.

      TRY.

          vg_edit = 0.

          CALL METHOD me->o_alv03->set_ready_for_input( EXPORTING i_ready_for_input = vg_edit ).

          CALL METHOD me->o_alv03->set_table_for_first_display
            EXPORTING
              is_layout       = gs_layout
              "i_structure_name = 'TY_SAIDA'
              is_variant      = ls_variant
              i_save          = 'A'
            CHANGING
              it_outtab       = me->it_saida03
              it_fieldcatalog = me->it_fieldcat_03.

        CATCH cx_root.
      ENDTRY.


      CALL METHOD me->o_alv03->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      IF lines( t_rows ) > 0.
        CALL METHOD me->o_alv03->set_selected_rows
          EXPORTING
            it_index_rows = t_rows.
      ENDIF.
    ENDIF.

    "l_title = |Saldos a serem constituídos PCLD|.


  ENDMETHOD.

  METHOD on_f4_out3.
    DATA: wa_saida03 TYPE ty_analitico,
          lt_return  TYPE TABLE OF ddshretval.

    FREE: lt_return.
    CLEAR: wa_saida03 .

*    CALL METHOD me->o_alv03->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    IF lines( t_rows ) > 0.
*      CALL METHOD me->o_alv03->set_selected_rows
*        EXPORTING
*          it_index_rows = t_rows.
*    ENDIF.

*    CALL METHOD me->o_alv03->refresh_table_display( ).

*    ASSIGN COMPONENT e_fieldname OF STRUCTURE wa_saida03 TO FIELD-SYMBOL(<fs_field>).
*    IF <fs_field> IS ASSIGNED.
**      CONDENSE ls_return-fieldval NO-GAPS.
**      <fs_field> = ls_return-fieldval.
**      SELECT SINGLE ddtext FROM zi_class_f4_zfit0232 WHERE domvalue_l = @wa_saida03-class  INTO @wa_saida03-class2.
*    ENDIF.
*    MODIFY me->it_saida03 FROM wa_saida03 INDEX es_row_no-row_id.


*    MODIFY me->it_saida03 FROM wa_saida03 INDEX es_row_no-row_id.

    CASE e_fieldname.
      WHEN 'CLASS'.

        CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
          EXPORTING
            tabname    = 'ZFIT0232'
            fieldname  = 'CLASS'
            dynpprog   = sy-repid
            dynpnr     = sy-dynnr
          TABLES
            return_tab = lt_return.

        IF lt_return IS NOT INITIAL.
          READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
          IF sy-subrc = 0.
            READ TABLE me->it_saida03 INTO wa_saida03 INDEX es_row_no-row_id.
            IF sy-subrc = 0.

              ASSIGN COMPONENT e_fieldname OF STRUCTURE wa_saida03 TO FIELD-SYMBOL(<fs_field>).
              IF <fs_field> IS ASSIGNED.
                CONDENSE ls_return-fieldval NO-GAPS.
                <fs_field> = ls_return-fieldval.
                SELECT SINGLE ddtext FROM zi_class_f4_zfit0232 WHERE domvalue_l = @wa_saida03-class  INTO @wa_saida03-class2.
              ENDIF.
              MODIFY me->it_saida03 FROM wa_saida03 INDEX es_row_no-row_id.
            ELSE.
              ASSIGN COMPONENT e_fieldname OF STRUCTURE wa_saida03 TO <fs_field>.
              IF <fs_field> IS ASSIGNED.
                CONDENSE ls_return-fieldval NO-GAPS.
                <fs_field> = ls_return-fieldval.
                SELECT SINGLE ddtext FROM zi_class_f4_zfit0232 WHERE domvalue_l = @wa_saida03-class  INTO @wa_saida03-class2.
              ENDIF.
              APPEND wa_saida03 TO me->it_saida03.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL METHOD me->o_alv03->refresh_table_display( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
