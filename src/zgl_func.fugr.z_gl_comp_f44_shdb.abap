************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 25.01.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Programa de automatização de compensação de fornec. *
*                  do modulo Comercialização-SIGAM (F-44)              *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 25.01.2008    Michely              Inicio               DEVK903420   *
* 25.01.2008    Marcus Barbara       Alteração            DEVK905373   *
* 28.07.2010    Marcus Barbara       Alteração            DEVK908709   *
************************************************************************

FUNCTION z_gl_comp_f44_shdb.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MODO) TYPE  BSEG-BUZID
*"     REFERENCE(I_RET) TYPE  BSEG-BUZID
*"  TABLES
*"      IT_LOTES STRUCTURE  ZGL003_COMP_F44
*"      IT_RETORNO STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  DATA: vl_cnt(2)           TYPE n,
        vl_cnt_p(4)         TYPE n,
        vl_valor(15)        TYPE c,
        vl_field            TYPE c LENGTH 60,
        vl_lifnr            LIKE zgl001_comp_f44-lifnr,
        vl_belnr            LIKE zgl001_comp_f44-belnr,
        vl_fordif           TYPE c LENGTH 1,
        vl_tpshdb           TYPE c LENGTH 2,
        vl_txt              TYPE c LENGTH 50,
        vl_mode             TYPE c LENGTH 1,
        vl_data             TYPE c LENGTH 8,
        vl_raz              TYPE c LENGTH 10,
        vl_raz_aux          TYPE c LENGTH 10,
        vl_mess(256)        TYPE c,
        vl_sp               TYPE c LENGTH 1,
        vl_np               TYPE c LENGTH 1,
        vl_sinal            TYPE c LENGTH 1,
        vl_valdoc           TYPE c LENGTH 1,
        vl_shkzg            LIKE bsik-shkzg,
        vl_wrbtr            LIKE bsik-wrbtr,
        vl_gsber            LIKE bsik-gsber,
        vl_dif              LIKE bsik-wrbtr,
        vl_part_res         LIKE bsik-wrbtr,
        vl_dif_f            LIKE bsik-wrbtr,
        vl_idex             TYPE sy-tabix,
        r_inter             LIKE RANGE OF bsik-wrbtr WITH HEADER LINE. " Exemplo zfib005.

  REFRESH: it_bdcdata, it_retorno, r_inter.

  CLEAR r_inter.
  r_inter-sign = 'I'.
  r_inter-option = 'BT'.
  r_inter-low  = '-1'.
  r_inter-high = '1'.
  APPEND r_inter.

  SELECT mandt bukrs belnr buzei lote lifnr waers budat umskz dmbtr processamento acerto
    FROM zgl001_comp_f44 AS z1
    INTO TABLE it_docbase
     FOR ALL ENTRIES IN it_lotes
   WHERE z1~bukrs EQ it_lotes-bukrs
     AND z1~lote  EQ it_lotes-lote.

  LOOP AT it_docbase INTO data(wa_docbase).
    DATA(_tabix) = sy-tabix.

     CLEAR: vl_belnr, vl_gsber.
     IF ( wa_docbase-buzei IS NOT INITIAL ) OR ( wa_docbase-buzei NE 0 ).
       SELECT SINGLE belnr gsber
         FROM bsik INTO (vl_belnr, vl_gsber)
        WHERE bukrs EQ wa_docbase-bukrs
          AND belnr EQ wa_docbase-belnr
          AND lifnr EQ wa_docbase-lifnr
          AND buzei EQ wa_docbase-buzei.
     ELSE.
       SELECT SINGLE belnr gsber
         FROM bsik INTO (vl_belnr, vl_gsber)
        WHERE bukrs EQ wa_docbase-bukrs
          AND belnr EQ wa_docbase-belnr
          AND lifnr EQ wa_docbase-lifnr
          AND shkzg EQ 'S'.
     ENDIF.

     IF ( vl_belnr is NOT INITIAL ) AND ( vl_gsber is NOT INITIAL ).
       wa_docbase-gsber = vl_gsber.
       MODIFY it_docbase from wa_docbase INDEX _tabix.
     ENDIF.
  ENDLOOP.

  SELECT DISTINCT mandt bukrs lote
    FROM zgl001_comp_f44 AS z1
    INTO TABLE it_complote
     FOR ALL ENTRIES IN it_lotes
   WHERE z1~bukrs EQ it_lotes-bukrs
     AND z1~lote  EQ it_lotes-lote.

  it_documentos[] = it_docbase[].
  CLEAR vl_cnt_p.
  LOOP AT it_complote INTO wa_lotes.
    CLEAR: vl_valdoc, vl_dif, vl_part_res, vl_dif_f.
    LOOP AT it_documentos INTO wa_documentos
                WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.

      CLEAR: vl_belnr, vl_txt, vl_wrbtr.
      IF ( wa_documentos-buzei IS NOT INITIAL ) OR ( wa_documentos-buzei NE 0 ).
        SELECT SINGLE belnr wrbtr shkzg
          INTO (vl_belnr, vl_wrbtr, vl_shkzg)
          FROM bsik
         WHERE bukrs EQ wa_documentos-bukrs
           AND belnr EQ wa_documentos-belnr
"           AND gjahr EQ wa_documentos-gjahr
           AND lifnr EQ wa_documentos-lifnr
           AND buzei EQ wa_documentos-buzei.
      ELSE.
        SELECT SINGLE belnr wrbtr shkzg
          INTO (vl_belnr, vl_wrbtr, vl_shkzg)
          FROM bsik
         WHERE bukrs EQ wa_documentos-bukrs
           AND belnr EQ wa_documentos-belnr
"           AND gjahr EQ wa_documentos-gjahr
           AND lifnr EQ wa_documentos-lifnr
           AND shkzg EQ 'S'.
      ENDIF.
      IF vl_shkzg EQ 'H'.
        vl_dif = vl_dif + ( vl_wrbtr * -1 ).
      ELSE.
        vl_dif = vl_dif + vl_wrbtr.
      ENDIF.
      IF wa_documentos-processamento EQ 'P'.
        PERFORM f_busca_sinal USING wa_documentos-bukrs
                                    wa_documentos-lifnr
                                    wa_documentos-belnr
                                    wa_documentos-buzei
                           CHANGING vl_sinal.
        IF vl_sinal EQ '-'.
          vl_part_res = vl_part_res + ( wa_documentos-dmbtr * -1 ).
        ELSE.
          vl_part_res = vl_part_res + wa_documentos-dmbtr.
        ENDIF.
      ENDIF.
      "Caso o documento não seja localizado
      IF vl_belnr IS INITIAL.
        CONCATENATE 'O documento'
                    wa_documentos-belnr
                    'item'
                    wa_documentos-buzei
                    'não foi localizado.' INTO vl_txt
                    SEPARATED BY space.
        PERFORM f_log_retorno USING 'E' vl_txt.
        vl_valdoc = 'N'.
        EXIT.
      ENDIF.

    ENDLOOP.
*    if vl_part_res lt 0.
    vl_part_res = vl_part_res * -1.
*    endif.
    IF vl_part_res NE 0.
      vl_dif_f = vl_part_res + vl_dif.

      "Comentando CS2018002048 29.10.2018 - Ini
*      IF vl_dif_f IN r_inter.
*        SORT: it_documentos BY processamento dmbtr DESCENDING.
*        READ TABLE it_documentos INTO wa_documentos WITH KEY processamento = 'P'
*                                                             BINARY SEARCH.
*        PERFORM f_busca_sinal USING wa_documentos-bukrs
*                                    wa_documentos-lifnr
*                                    wa_documentos-belnr
*"                                    wa_documentos-gjahr
*                                    wa_documentos-buzei
*                           CHANGING vl_sinal.
*        IF ( vl_dif_f GT 0 ) AND ( vl_sinal EQ '-').
*          vl_dif_f = wa_documentos-dmbtr - vl_dif_f.
*        ELSEIF ( vl_dif_f GT 0 ) AND ( vl_sinal EQ '' ).
*          vl_dif_f = wa_documentos-dmbtr + vl_dif_f.
*        ELSEIF ( vl_dif_f LT 0 ) AND ( vl_sinal EQ '' ).
*          vl_dif_f = wa_documentos-dmbtr + vl_dif_f.
*        ELSEIF ( vl_dif_f LT 0 ) AND ( vl_sinal EQ '-' ).
*          vl_dif_f = wa_documentos-dmbtr - vl_dif_f.
*        ENDIF.
*        IF vl_dif_f GT 0.
*          UPDATE zgl001_comp_f44 SET dmbtr = vl_dif_f
*                               WHERE bukrs EQ wa_documentos-bukrs
*                                 AND belnr EQ wa_documentos-belnr
*                                 AND buzei EQ wa_documentos-buzei
*                                 AND lote  EQ wa_documentos-lote.
*          wa_documentos-dmbtr = vl_dif_f.
*          MODIFY TABLE it_documentos FROM wa_documentos.
*          it_docbase[] = it_documentos[].
*        ENDIF.
*      ENDIF.
      "Comentando CS2018002048 29.10.2018 - Fim

    ELSE.
      "Comentando CS2018002048 29.10.2018 - Ini
*      IF vl_dif IN r_inter.
*        SORT: it_documentos BY belnr buzei.
*        LOOP AT it_documentos INTO wa_documentos.
*          vl_idex = sy-tabix.
*          PERFORM f_busca_sinal USING wa_documentos-bukrs
*                                      wa_documentos-lifnr
*                                      wa_documentos-belnr
*"                                      wa_documentos-gjahr
*                                      wa_documentos-buzei
*                             CHANGING vl_sinal.
*          IF ( vl_dif GT 0 ) AND ( vl_sinal EQ '').
*            EXIT.
*          ENDIF.
*          IF ( vl_dif LT 0 ) AND ( vl_sinal EQ '-').
*            vl_dif = vl_dif * -1.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
*        IF vl_dif NE 0.
*          UPDATE zgl001_comp_f44 SET dmbtr = vl_dif
*                                     processamento = 'P'
*                               WHERE bukrs EQ wa_documentos-bukrs
*                                 AND belnr EQ wa_documentos-belnr
*                                 AND buzei EQ wa_documentos-buzei
*                                 AND lote  EQ wa_documentos-lote.
*
*          SELECT *
*            FROM zgl001_comp_f44
*            INTO TABLE it_documentos
*           WHERE bukrs EQ wa_lotes-bukrs
*             AND lote  EQ wa_lotes-lote.
*
*          it_docbase[] = it_documentos[].
*        ENDIF.
*      ENDIF.
      "Comentando CS2018002048 29.10.2018 - Fim
    ENDIF.
    CLEAR vl_txt.

    IF vl_valdoc NE 'N'. "Valido se documento existe na BSIK
      CLEAR: vl_raz, vl_lifnr, vl_raz_aux.
      SORT it_documentos BY lote lifnr umskz.
      LOOP AT it_documentos INTO wa_documentos
        WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.
        IF vl_raz_aux NE wa_documentos-umskz.
          CONCATENATE vl_raz
                      wa_documentos-umskz
                      INTO vl_raz.
        ENDIF.
*        Identifico se existe mais de um fornecedor no lote
        IF vl_lifnr IS INITIAL.
          vl_lifnr = wa_documentos-lifnr.
        ENDIF.
        IF vl_lifnr EQ wa_documentos-lifnr.
          vl_fordif = 'S'.
        ELSE.
          vl_fordif = 'N'.
        ENDIF.
        vl_raz_aux = wa_documentos-umskz.
      ENDLOOP.

      SORT it_documentos BY lote lifnr processamento.
      READ TABLE it_documentos INDEX 1 INTO wa_documentos.

      CONCATENATE wa_documentos-budat+6(2)
                  wa_documentos-budat+4(2)
                  wa_documentos-budat(4) INTO vl_data.

      PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0131',
                  ' ' 'BDC_OKCODE'                '/00',
                  ' ' 'RF05A-AGKON'               wa_documentos-lifnr,
                  ' '	'BKPF-BUDAT'                vl_data,
                  ' '	'BKPF-MONAT'                wa_documentos-budat+4(2),
                  ' ' 'BKPF-BUKRS'                wa_documentos-bukrs,
                  ' ' 'BKPF-WAERS'                wa_documentos-waers,
                  ' ' 'RF05A-AGUMS'               vl_raz,
                  ' '	'RF05A-XNOPS'               'X', "Cód.: selecionar PAs que não sejam faturamento especial?
                  ' ' 'RF05A-XPOS1(03)'           'X'.
      CASE vl_fordif.
*---------------------------------------------------------------------
        WHEN 'S'. "Unico fornecedor
          PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0731'.
          CLEAR: vl_cnt, vl_belnr.
          SORT it_documentos BY lote belnr buzei processamento.
          LOOP AT it_documentos INTO wa_documentos WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.
            IF vl_belnr IS INITIAL.
              vl_belnr = '9999999999'.
            ENDIF.
            IF vl_belnr NE wa_documentos-belnr.

              vl_cnt = vl_cnt + 1.
              IF vl_cnt GT c_doc.
                vl_cnt = 1.
                PERFORM f_bdc_insert USING :
                      ' '	'BDC_OKCODE'                '=SL2'.
                PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0608',
                       ' '  'BDC_OKCODE'                '=ENTR',
                       ' '  'RF05A-XPOS1(02)'           'X'.
                PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0731'.
              ENDIF.

              CONCATENATE 'RF05A-SEL01('
                          vl_cnt
                          ')' INTO vl_field.
              PERFORM f_bdc_insert USING :
                          ' '  vl_field  wa_documentos-belnr.
              vl_belnr = wa_documentos-belnr.
*           Controlo o numero de partidas para chamar nova tela (17).
            ENDIF.
          ENDLOOP.
          PERFORM f_bdc_insert USING :
                      ' '	'BDC_OKCODE'                '=PA'.

          CLEAR: wa_documentos, vl_cnt, vl_sp, vl_np, vl_belnr.
          LOOP AT it_documentos INTO wa_documentos
                               WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.
            IF vl_belnr IS INITIAL.
              vl_belnr = '9999999999'.
            ENDIF.

            SELECT SINGLE name1
              FROM lfa1
              INTO vl_txt
             WHERE lifnr EQ wa_documentos-lifnr.

            CONCATENATE wa_documentos-lote
                        '-'
                        vl_txt INTO vl_txt.
            IF vl_cnt IS INITIAL.
              vl_cnt = vl_cnt + 1.
            ENDIF.
            vl_valor = wa_documentos-dmbtr.
            TRANSLATE vl_valor USING '.,'.
            IF vl_cnt_p IS INITIAL.
              vl_cnt_p = vl_cnt_p + 1.
            ENDIF.
            IF vl_belnr NE wa_documentos-belnr.
*              if wa_documentos-processamento eq 'P'. "Baixa parcial
              IF vl_sp IS INITIAL.
                CONCATENATE 'DF05B-PSSKT('
                            vl_cnt
                            ')' INTO vl_field.
                PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
                            ' ' 'BDC_OKCODE'       '=REST',
                            ' ' 'BDC_SUBSCR'       'SAPDF05X',
                            ' ' 'RF05A-ABPOS'       '1'.
                PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
                            ' ' 'BDC_OKCODE'       '/00',
                            ' ' 'BDC_SUBSCR'       'SAPDF05X'.
                vl_sp = 1.
              ENDIF.
*              endif.
              PERFORM f_busca_itens USING wa_documentos-bukrs
                                          wa_documentos-lote
                                          wa_documentos-lifnr
                                          wa_documentos-belnr
"                                          wa_documentos-gjahr
                                          wa_documentos-buzei
                                 CHANGING vl_cnt vl_cnt_p.
              CLEAR vl_field.
            ENDIF.
            vl_belnr = wa_documentos-belnr.
          ENDLOOP.

*          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
*                      ' ' 'BDC_OKCODE'                '=PI',
*                      ' ' 'BDC_SUBSCR'                'SAPDF05X',
*                      ' ' 'BDC_CURSOR'                'RF05A-AKOBT',
*                      ' ' 'RF05A-ABPOS'                '1'.

          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'	'3100',
                      ' ' 'BDC_OKCODE'                '=TX',
                      ' ' 'BDC_SUBSCR'                'SAPDF05X'.
          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'   '2001',
                      ' ' 'BDC_OKCODE'                  '=GO',
                      ' ' 'RF05A-AUGTX'                 vl_txt.

          DATA(_desconto) = abap_false.
          IF vl_part_res NE 0.
            PERFORM f_desconto_dif USING vl_dif_f wa_lotes
                                CHANGING _desconto.
          else.
            PERFORM f_desconto_dif USING vl_dif wa_lotes
                                CHANGING _desconto.
          endif.

          IF _desconto EQ ABAP_FALSE.
            PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'	'3100',
                      ' ' 'BDC_OKCODE'       '=BU',
                      ' ' 'BDC_SUBSCR'        'SAPDF05X',
                      ' ' 'RF05A-ABPOS'      '1'.
          ENDIF.



*---------------------------------------------------------------------
        WHEN 'N'. " Varios fornecedores.
          PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0731',
                      ' '	'BDC_OKCODE'                '=SLK'.
          CLEAR: vl_cnt, vl_lifnr, vl_belnr.
          SORT it_documentos BY lote lifnr belnr buzei processamento.
          LOOP AT it_documentos INTO wa_documentos
            WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.
            IF vl_belnr IS INITIAL.
              vl_belnr = '9999999999'.
            ENDIF.
            IF vl_belnr NE wa_documentos-belnr.
              IF vl_lifnr IS INITIAL.
                vl_lifnr = wa_documentos-lifnr.
              ENDIF.
              IF vl_lifnr EQ wa_documentos-lifnr.
                vl_cnt = vl_cnt + 1.
                IF vl_cnt GT c_doc.
                  vl_cnt = 1.
                  PERFORM f_bdc_insert USING :
                        ' '	'BDC_OKCODE'                '=SL2'.
                  PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0608',
                        ' '	'BDC_OKCODE'                '=ENTR',
                        ' '	'RF05A-XPOS1(02)'           'X'.
                  PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0731'.
                ENDIF.

                CONCATENATE 'RF05A-SEL01('
                            vl_cnt
                            ')' INTO vl_field.
                PERFORM f_bdc_insert USING :
                            ' '  vl_field  wa_documentos-belnr.
*              delete it_documentos.
              ELSE.
                PERFORM f_bdc_insert USING :
                            ' '	'BDC_OKCODE'                'SLK'.
                CONCATENATE wa_dochead-budat+6(2)
                            wa_dochead-budat+4(2)
                            wa_dochead-budat(4) INTO vl_data.
                PERFORM f_bdc_insert USING : 'X' 'SAPMF05A'	'0710',
                            ' ' 'BDC_OKCODE'                '=PA',
                            ' ' 'RF05A-AGBUK'	              wa_documentos-bukrs,
                            ' ' 'RF05A-AGKON'	              wa_documentos-lifnr,
                            ' ' 'RF05A-AGKOA'	              'K',
                            ' ' 'RF05A-AGUMS'	              vl_raz,
                            ' ' 'RF05A-XNOPS'	              'X',
                            ' ' 'RF05A-XPOS1(03)'	          'x'.
                CLEAR: vl_cnt.
                vl_lifnr = wa_documentos-lifnr.
                vl_cnt = vl_cnt + 1.
                CONCATENATE 'RF05A-SEL01('
                            vl_cnt
                            ')' INTO vl_field.
                PERFORM f_bdc_insert USING : 'X' 'SAPMF05A' '0731',
                            ' '  vl_field  wa_documentos-belnr.
                DELETE it_documentos.
              ENDIF.
*           Controlo o numero de documentos para chamar nova tela.
            ENDIF.
            vl_belnr = wa_documentos-belnr.
          ENDLOOP.

          PERFORM f_bdc_insert USING :
                      ' '	'BDC_OKCODE'                '=PA'.
*          perform f_bdc_insert using : 'X' 'SAPMF05A'  '0710',
*                      ' ' 'BDC_OKCODE'                '=PA'.

          it_documentos[] = it_docbase[].

          SORT it_documentos BY lote lifnr belnr buzei processamento.
          CLEAR: wa_documentos, vl_cnt, vl_sp, vl_np, vl_belnr.
          LOOP AT it_documentos INTO wa_documentos
            WHERE bukrs = wa_lotes-bukrs AND lote = wa_lotes-lote.
            IF vl_belnr IS INITIAL.
              vl_belnr = '9999999999'.
            ENDIF.
            SELECT SINGLE name1
              FROM lfa1
              INTO vl_txt
             WHERE lifnr EQ wa_documentos-lifnr.

            CONCATENATE wa_documentos-lote
                        '-'
                        vl_txt INTO vl_txt.
            IF vl_cnt IS INITIAL.
              vl_cnt = vl_cnt + 1.
            ENDIF.
            IF vl_cnt_p IS INITIAL.
              vl_cnt_p = vl_cnt_p + 1.
            ENDIF.
            vl_valor = wa_documentos-dmbtr.
            TRANSLATE vl_valor USING '.,'.
            IF vl_belnr NE wa_documentos-belnr.
*              if wa_documentos-processamento eq 'P'. "Varios fornecedores e baixa parcial
              IF vl_sp IS INITIAL.
                CONCATENATE 'DF05B-PSSKT('
                vl_cnt
                ')' INTO vl_field.
                PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
                      ' ' 'BDC_OKCODE'       '=REST',
                      ' ' 'BDC_SUBSCR'       'SAPDF05X',
                      ' ' 'RF05A-ABPOS'       '1'.
                PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
                      ' ' 'BDC_OKCODE'       '/00',
                      ' ' 'BDC_SUBSCR'       'SAPDF05X'.
                vl_sp = 1.
              ENDIF.
*              endif.
              PERFORM f_busca_itens USING wa_documentos-bukrs
                                          wa_documentos-lote
                                          wa_documentos-lifnr
                                          wa_documentos-belnr
"                                          wa_documentos-gjahr
                                          wa_documentos-buzei
                                 CHANGING vl_cnt vl_cnt_p.
            ENDIF.
            vl_belnr = wa_documentos-belnr.
          ENDLOOP.
          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'	'3100',
                      ' ' 'BDC_OKCODE'                '=TX',
                      ' ' 'BDC_SUBSCR'                'SAPDF05X'.
          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'   '2001',
                      ' ' 'BDC_OKCODE'                  '=GO',
                      ' ' 'RF05A-AUGTX'                 vl_txt.

*          PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'  '3100',
*                      ' ' 'BDC_OKCODE'                '=PI',
*                      ' ' 'BDC_SUBSCR'                'SAPDF05X',
*                      ' ' 'BDC_CURSOR'                'RF05A-AKOBT',
*                      ' ' 'RF05A-ABPOS'                '1'.


*          if it_part_residual[] is not initial.
*            clear vl_cnt.
*            perform f_bdc_insert using : 'X' 'SAPDF05X'  '4201',
*                        ' ' 'BDC_CURSOR'                'DF05B-TREST(01)',
*                        ' ' 'BDC_OKCODE'                '/00'.
*            sort it_part_residual by lifnr.
*            loop at it_part_residual into wa_part_residual.
*              vl_cnt = vl_cnt + 1.
*              vl_valor = wa_part_residual-dmbtr.
*              translate vl_valor using '.,'.
*              concatenate 'DF05B-TREST('
*                          vl_cnt
*                          ')' into vl_field.
*              perform f_bdc_insert using : ' ' vl_field  vl_valor.
*              concatenate 'DF05B-DFKRE('
*                          vl_cnt
*                          ')' into vl_field.
*              perform f_bdc_insert using : ' ' vl_field  wa_part_residual-lifnr.
*            endloop.
*            perform f_bdc_insert using : 'X' 'SAPDF05X'  '4201',
*                        ' ' 'BDC_OKCODE'                '=BU'.
*          else.

          _desconto = abap_false.
          IF vl_part_res NE 0.
            PERFORM f_desconto_dif USING vl_dif_f wa_lotes
                                CHANGING _desconto.
          else.
            PERFORM f_desconto_dif USING vl_dif wa_lotes
                                CHANGING _desconto.
          endif.

          IF _desconto EQ ABAP_FALSE.
            PERFORM f_bdc_insert USING : 'X' 'SAPDF05X'	'3100',
                        ' ' 'BDC_OKCODE'                  '=BU',
                        ' ' 'BDC_SUBSCR'                  'SAPDF05X',
                        ' ' 'RF05A-ABPOS'                  '1'.
          ENDIF.
*          endif.
      ENDCASE.

      CALL TRANSACTION 'F-44' USING   it_bdcdata
                              MODE    i_modo
                              UPDATE  'S'
                     MESSAGES INTO    it_retorno.

      READ TABLE it_retorno INTO wa_message
                        WITH KEY  msgtyp = 'E'.
      IF sy-subrc EQ 0.
        PERFORM f_busca_mess USING wa_message-msgid
              wa_message-msgnr
              wa_message-msgv1
              wa_message-msgv2
              wa_message-msgv3
              wa_message-msgv4
        CHANGING vl_mess.
        PERFORM f_log_retorno USING wa_message-msgtyp vl_mess.
      ELSE.
        READ TABLE it_retorno INTO wa_message
                          WITH KEY  msgtyp = 'W'.
        IF sy-subrc EQ 0 AND wa_message-msgnr NE '149' AND wa_message-msgnr NE '014'.
          PERFORM f_busca_mess USING wa_message-msgid
                wa_message-msgnr
                wa_message-msgv1
                wa_message-msgv2
                wa_message-msgv3
                wa_message-msgv4
          CHANGING vl_mess.
          PERFORM f_log_retorno USING 'E' vl_mess.
        ELSE.
          READ TABLE it_retorno INTO wa_message
                            WITH KEY  msgtyp = 'A'.
          IF sy-subrc EQ 0.
            PERFORM f_busca_mess USING wa_message-msgid
                  wa_message-msgnr
                  wa_message-msgv1
                  wa_message-msgv2
                  wa_message-msgv3
                  wa_message-msgv4
            CHANGING vl_mess.
            PERFORM f_log_retorno USING 'E' vl_mess.
          ELSE.
            IF ( NOT it_retorno[] IS INITIAL ).
              LOOP AT it_retorno INTO wa_message.

              ENDLOOP.
              PERFORM f_busca_mess USING wa_message-msgid
                    wa_message-msgnr
                    wa_message-msgv1
                    wa_message-msgv2
                    wa_message-msgv3
                    wa_message-msgv4
              CHANGING vl_mess.
              PERFORM f_log_retorno USING wa_message-msgtyp vl_mess.

*              if i_ret is not initial.
*                if wa_message-msgnr eq 312.
*                  if wa_message-msgv1 is not initial.
*                    submit ZRFI001_START_RFC
*                      with p_belnr = wa_message-msgv1
*                      and return.
*                  endif.
*                endif.
*              endif.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF. "Validação se documento existe na BSIK
  ENDLOOP.

  SELECT *
    FROM zgl002_comp_f44
    INTO TABLE it_retorno_rfc
     FOR ALL ENTRIES IN it_complote
   WHERE bukrs EQ it_complote-bukrs
     AND lote  EQ it_complote-lote.

  IF ( it_retorno_rfc[] IS not INITIAL ).

    CALL FUNCTION 'Z_FI_OUTBOUND_LOTE_COMPRA_ADD'
      TABLES
        return = it_retorno_rfc.

    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
