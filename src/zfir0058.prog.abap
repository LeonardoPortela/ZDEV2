*&------------------------------------------------------------------------*
*& Report  ZFIR0058
*& Carregar dados SFTP Blooberg para tabele ZFIT0058
*&-*----------------------------------------------------------------------*
* Autor      : Camila Brand                              Data: 17.10.2014 *
* Observações: Desenvolvimento inicial do Programa                        *
*&------------------------------------------------------------------------*

REPORT  zfir0058.

TYPE-POOLS: truxs.
TABLES : zfit0083.

*&---------------------------------------------------------------------*
*& Inicio
*&---------------------------------------------------------------------*
" DEV - \\SRVVM146\BloombergDEV$\Recebidos
" PRD - \\172.12.12.180\bloomberg\Recebidos
* No servidor SRVVM146 existe um serviço do windows feito em C# pelo Vinicius, que conecta via FTP
* no servidor da BBG transfere os arquivos para o diretorio do SAP /usr/sap/BLOOMBERG/RECEBIDOS

*TYPES: TY_ARQUIVO(600)  TYPE C.
*V_RESULT               TYPE I,
*DIR_ARQ                TYPE STRING,
*T_ARQUIVO              TYPE TABLE OF TY_ARQUIVO,
*W_ARQUIVO              TYPE TY_ARQUIVO,
*TIPO_LINHA             TYPE I,
*VG_LOTE                TYPE I,

TYPES BEGIN OF ty_split.
TYPES: valor TYPE char200.
TYPES END OF ty_split.

DATA:
  p_dir(50)              TYPE c,
  v_cnpj_emp(18)         TYPE c,
  v_cnpj_arg(13)         TYPE c,
  p_dir_dest(50)         TYPE c,
  v_result               TYPE i,
  dlist                  LIKE epsfili OCCURS 0 WITH HEADER LINE,
  lv_nome_arquivo_rec    TYPE string,
  lv_nome_arquivo_proc   TYPE string,
  lv_nome_arquivo        TYPE string,
  t_file                 TYPE TABLE OF zarq_bloomberg WITH HEADER LINE,
  wa_t_file              TYPE zarq_bloomberg,
  t_split                TYPE TABLE OF ty_split WITH HEADER LINE,
  v_filename_string      TYPE rlgrap-filename,
  v_filename_string_proc TYPE rlgrap-filename,
  i                      TYPE i,
  wa_zfit0083            TYPE zfit0083,
  wa_zfit0083_proc       TYPE zfit0083,
  it_zfit0083            TYPE TABLE OF zfit0083,
  it_zfit0083_proc       TYPE TABLE OF zfit0083,
  string                 TYPE string,
  fin                    TYPE string,
  lv_c                   TYPE c,
  flag                   TYPE c,
  lv_len                 TYPE i,
  count                  TYPE i,
  v_rev_trade            TYPE zfit0083-rev_trade.


*&---------------------------------------------------------------------*
*& INICIO PROCESSAMENTO
*&---------------------------------------------------------------------*

"Para Execução em backgound (jobs) """"""""""""""""""""""""""""
IF sy-batch EQ abap_true.
  TRY .
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
    CATCH zcx_job.
      e_qtd = 1.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.
ENDIF.


p_dir      = '/usr/sap/BLOOMBERG/RECEBIDOS'.
p_dir_dest = '/usr/sap/BLOOMBERG/PROCESSADOS'.


CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
  EXPORTING
    dir_name               = '/usr/sap/BLOOMBERG/RECEBIDOS'
  TABLES
    dir_list               = dlist
  EXCEPTIONS
    invalid_eps_subdir     = 1
    sapgparam_failed       = 2
    build_directory_failed = 3
    no_authorization       = 4
    read_directory_failed  = 5
    too_many_read_errors   = 6
    empty_directory_list   = 7
    OTHERS                 = 8.

SORT: dlist      BY name.

IF sy-subrc = 0 .

  LOOP AT dlist .

    WRITE:/ dlist-name.

    CONCATENATE  p_dir       '/'  dlist-name INTO v_filename_string.
    CONCATENATE  p_dir_dest  '/'  dlist-name INTO v_filename_string_proc.

    MOVE v_filename_string      TO lv_nome_arquivo.
    MOVE v_filename_string      TO lv_nome_arquivo_rec.
    MOVE v_filename_string_proc TO lv_nome_arquivo_proc.


    " Para na pegar possiveis enter/return WITH WINDOWS LINEFEED.
    OPEN DATASET lv_nome_arquivo FOR INPUT IN TEXT MODE ENCODING NON-UNICODE WITH WINDOWS LINEFEED.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: t_file[].
    DO.
      READ DATASET lv_nome_arquivo INTO t_file.
      IF sy-subrc  IS INITIAL.
        APPEND t_file.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CLOSE DATASET lv_nome_arquivo.

    LOOP AT t_file INTO wa_t_file .

      CLEAR:string, lv_len, lv_c, fin, count, t_split.

      MOVE wa_t_file-linha TO string .
      CLEAR wa_t_file-linha.


      lv_len = numofchar( string ).

      DO lv_len TIMES.
        lv_c = string+count(1).
        IF lv_c = '"'.
          IF flag = ' '.
            flag = 'X'.
          ELSE.
            flag = ' '.
          ENDIF.
        ENDIF.

        IF flag = 'X' AND lv_c = ','.
          REPLACE lv_c WITH space INTO lv_c.
        ENDIF.
        CONCATENATE fin lv_c INTO fin.
        count = count + 1.
      ENDDO.

      MOVE fin TO wa_t_file-linha.


      SPLIT wa_t_file-linha AT ',' INTO TABLE t_split.

      LOOP AT t_split .

        CASE sy-tabix.

          WHEN 1.
            wa_zfit0083-mandt = sy-mandt.
            wa_zfit0083-msg_type          = t_split-valor.
          WHEN 2.
            wa_zfit0083-deal_type         = t_split-valor.
          WHEN 3.
            wa_zfit0083-side              = t_split-valor.
          WHEN 4.
            wa_zfit0083-product           = t_split-valor.
          WHEN 5.
            wa_zfit0083-source_ref        = t_split-valor.
          WHEN 6.
            wa_zfit0083-trans_type        = t_split-valor.
          WHEN 7.
            wa_zfit0083-rev_trade         = t_split-valor.
          WHEN 8.
            wa_zfit0083-trade_id          = t_split-valor.
          WHEN 9.
            wa_zfit0083-block_id          = t_split-valor.
          WHEN 10.
            wa_zfit0083-trader_id         = t_split-valor.
          WHEN 11.
            wa_zfit0083-trader_name       = t_split-valor.
          WHEN 12.
            wa_zfit0083-counterparty_id   = t_split-valor.
          WHEN 13.
            wa_zfit0083-counterparty_nam  = t_split-valor.
          WHEN 14.
            wa_zfit0083-date_of_deal      = t_split-valor.
          WHEN 15.
            wa_zfit0083-time_of_deal      = t_split-valor.
          WHEN 16.
            wa_zfit0083-trade_date        = t_split-valor.
          WHEN 17.
            wa_zfit0083-date_confirmed    = t_split-valor.
          WHEN 18.
            wa_zfit0083-time_confirmed    = t_split-valor.
          WHEN 19.
            wa_zfit0083-cont_part_deal_c  = t_split-valor.
          WHEN 20.
            wa_zfit0083-cont_part_name    = t_split-valor.
          WHEN 21.
            wa_zfit0083-usr_ident_1       = t_split-valor.
          WHEN 22.
            wa_zfit0083-usr_ident_2       = t_split-valor.
          WHEN 23.
            wa_zfit0083-usr_ident_3       = t_split-valor.
          WHEN 24.
            wa_zfit0083-usr_ident_4       = t_split-valor.
          WHEN 25.
            wa_zfit0083-brok_deal_code    = t_split-valor.
          WHEN 26.
            wa_zfit0083-brok_name         = t_split-valor.
          WHEN 27.
            TRY .
                wa_zfit0083-currency_1        = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-currency_1        = 0.
            ENDTRY.
          WHEN 28.
            TRY .
                wa_zfit0083-currency_2        = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-currency_2        = 0.
            ENDTRY.
          WHEN 29.
            TRY .
                wa_zfit0083-amount_dealt      = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-amount_dealt      = 0.
            ENDTRY.
          WHEN 30.
            TRY .
                wa_zfit0083-dealt_currency    = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-dealt_currency    = 0.
            ENDTRY.
          WHEN 31.
            TRY .
                wa_zfit0083-counter_amount    = t_split-valor.
              CATCH cx_root.
                wa_zfit0083-counter_amount    = 0.
            ENDTRY.

          WHEN 32.
            TRY .
                wa_zfit0083-counter_currency  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-counter_currency  = 0.
            ENDTRY.
          WHEN 33.
            TRY .
                wa_zfit0083-forward_pn        = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-forward_pn        = 0.
            ENDTRY.
          WHEN 34.
            TRY .
                wa_zfit0083-far_amount_dealt  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-far_amount_dealt  = 0.
            ENDTRY.
          WHEN 35.
            TRY .
                wa_zfit0083-far_currency_dea  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-far_currency_dea  = 0.
            ENDTRY.
          WHEN 36.
            TRY .
                wa_zfit0083-far_counter_amou  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-far_counter_amou  = 0.
            ENDTRY.
          WHEN 37.
            TRY .
                wa_zfit0083-far_counter_curr  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-far_counter_curr  = 0.
            ENDTRY.
          WHEN 38.
            TRY .
                wa_zfit0083-forward_points_f  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-forward_points_f  = 0.
            ENDTRY.
          WHEN 39.
            TRY .
                wa_zfit0083-spot_basis_rate   = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-spot_basis_rate   = 0.
            ENDTRY.
          WHEN 40.
            wa_zfit0083-deposit_rate      = t_split-valor.
          WHEN 41.
            wa_zfit0083-day_count         = t_split-valor.
          WHEN 42.
            wa_zfit0083-new_or_rollover   = t_split-valor.
          WHEN 43.
            wa_zfit0083-volume_of_intere  = t_split-valor.
          WHEN 44.
            TRY .
                wa_zfit0083-exch_rat_period1  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-exch_rat_period1 = 0.
            ENDTRY.
          WHEN 45.
            wa_zfit0083-date_period_1     = t_split-valor.
          WHEN 46.
            wa_zfit0083-tenor_period_1    = t_split-valor.
          WHEN 47.
            wa_zfit0083-fix_date_peri1    = t_split-valor.
          WHEN 48.
            wa_zfit0083-fix_source_peri1  = t_split-valor.
          WHEN 49.
            TRY .
                wa_zfit0083-settle_currency   = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-settle_currency   = 0.
            ENDTRY.
          WHEN 50.
            TRY .
                wa_zfit0083-swap_rate         = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-swap_rate         = 0.
            ENDTRY.
          WHEN 51.
            TRY .
                wa_zfit0083-exch_rat_period2  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-exch_rat_period2  = 0.
            ENDTRY.
          WHEN 52.
            wa_zfit0083-date_period_2     = t_split-valor.
          WHEN 53.
            wa_zfit0083-tenor_period_2    = t_split-valor.
          WHEN 54.
            wa_zfit0083-fix_date_peri2    = t_split-valor.
          WHEN 55.
            wa_zfit0083-fix_source_peri2  = t_split-valor.
          WHEN 56.
            wa_zfit0083-split_teno_curr1  = t_split-valor.
          WHEN 57.
            wa_zfit0083-split_date_curr1  = t_split-valor.
          WHEN 58.
            wa_zfit0083-split_teno_curr2  = t_split-valor.
          WHEN 59.
            wa_zfit0083-split_date_curr2  = t_split-valor.
          WHEN 60.
            wa_zfit0083-comment_text      = t_split-valor.
          WHEN 61.
            wa_zfit0083-note_name_1       = t_split-valor.
          WHEN 62.
            wa_zfit0083-note_text_1       = t_split-valor.
          WHEN 63.
            wa_zfit0083-note_name_2       = t_split-valor.
          WHEN 64.
            wa_zfit0083-note_text_2       = t_split-valor.
          WHEN 65.
            wa_zfit0083-note_name_3       = t_split-valor.
          WHEN 66.
            wa_zfit0083-note_text_3       = t_split-valor.
          WHEN 67.
            wa_zfit0083-note_name_4       = t_split-valor.
          WHEN 68.
            wa_zfit0083-note_text_4       = t_split-valor.
          WHEN 69.
            wa_zfit0083-note_name_5       = t_split-valor.
          WHEN 70.
            wa_zfit0083-note_text_5       = t_split-valor.
          WHEN 71.
            wa_zfit0083-comp_quote1       = t_split-valor.
          WHEN 72.
            wa_zfit0083-comp_quot_deal1   = t_split-valor.
          WHEN 73.
            wa_zfit0083-comp_quote2       = t_split-valor.
          WHEN 74.
            wa_zfit0083-comp_quot_deal2   = t_split-valor.
          WHEN 75.
            wa_zfit0083-comp_quote3       = t_split-valor.
          WHEN 76.
            wa_zfit0083-comp_quot_deal3   = t_split-valor.
          WHEN 77.
            wa_zfit0083-comp_quote4       = t_split-valor.
          WHEN 78.
            wa_zfit0083-comp_quot_deal4   = t_split-valor.
          WHEN 79.
            wa_zfit0083-comp_quote5       = t_split-valor.
          WHEN 80.
            wa_zfit0083-comp_quot_deal5   = t_split-valor.
          WHEN 81.
            wa_zfit0083-portfolio         = t_split-valor.
          WHEN 82.
            wa_zfit0083-alloc_account     = t_split-valor.

            v_cnpj_emp := wa_zfit0083-alloc_account(18).
            v_cnpj_arg := wa_zfit0083-alloc_account(13).

            wa_zfit0083-bukrs = ''.


            CASE v_cnpj_emp .

              WHEN '00.315.457/0001-95'. "AGRO
                wa_zfit0083-bukrs = '0015'.

              WHEN '77.294.254/0001-94'. "Amaggi
                wa_zfit0083-bukrs = '0001'.

              WHEN '84.590.892/0001-18'. "Hermasa
                wa_zfit0083-bukrs = '0010'.

              WHEN '11.338.257/0001-74'. "UNITAPAJOS
                wa_zfit0083-bukrs = '0039'.

              WHEN '10.962.697/0001-35'. "A&LDC
                wa_zfit0083-bukrs = '0035'.

              WHEN '15.143.827/0001-21'. "A&LDC T.P
                wa_zfit0083-bukrs = '0038'.

              WHEN '05.683.277/0001-80'.  "Otelhar
                wa_zfit0083-bukrs = '0050'.

              WHEN OTHERS.
                wa_zfit0083-bukrs = '0001'.

            ENDCASE.

            CASE v_cnpj_arg .
              WHEN   '30-71161551-9'. "ARGENTINA CUIT (CNPJ)
                wa_zfit0083-bukrs = '0100'.
            ENDCASE.

            v_cnpj_emp = ''.
            v_cnpj_arg = ''.

          WHEN 83.
            wa_zfit0083-alloc_descriptio  = t_split-valor.
          WHEN 84.
            wa_zfit0083-alloc_custodian   = t_split-valor.
          WHEN 85.
            wa_zfit0083-prim_broker_name  = t_split-valor.
          WHEN 86.
            TRY .
                wa_zfit0083-ref_spot_rate     = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-ref_spot_rate     = 0.
            ENDTRY.
          WHEN 87.
            TRY .
                wa_zfit0083-ref_rate_period1  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-ref_rate_period1  = 0.
            ENDTRY.
          WHEN 88.
            TRY .
                wa_zfit0083-ref_rate_period2  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-ref_rate_period2  = 0.
            ENDTRY.
          WHEN 89.
            wa_zfit0083-pay_currency      = t_split-valor.
          WHEN 90.
            wa_zfit0083-pay_swift_code    = t_split-valor.
          WHEN 91.
            wa_zfit0083-pay_account_numb  = t_split-valor.
          WHEN 92.
            wa_zfit0083-pay_bank          = t_split-valor.
          WHEN 93.
            wa_zfit0083-pay_branch        = t_split-valor.
          WHEN 94.
            wa_zfit0083-pay_beneficiary   = t_split-valor.
          WHEN 95.
            wa_zfit0083-pay_special_inst  = t_split-valor.
          WHEN 96.
            wa_zfit0083-rec_currency      = t_split-valor.
          WHEN 97.
            wa_zfit0083-rec_swift_code    = t_split-valor.
          WHEN 98.
            wa_zfit0083-rec_account_numb  = t_split-valor.
          WHEN 99.
            wa_zfit0083-receiving_bank    = t_split-valor.
          WHEN 100.
            wa_zfit0083-receiving_branch  = t_split-valor.
          WHEN 101.
            wa_zfit0083-rece_beneficiary  = t_split-valor.
          WHEN 102.
            wa_zfit0083-rec_special_inst  = t_split-valor.
          WHEN 103.
            TRY .
                wa_zfit0083-spot_rate_mid_po  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-spot_rate_mid_po  = 0.
            ENDTRY.
          WHEN 104.
            TRY .
                wa_zfit0083-all_in_rate_near  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-all_in_rate_near  = 0.
            ENDTRY.
          WHEN 105.
            TRY .
                wa_zfit0083-near_leg_forward  = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-near_leg_forward  = 0.
            ENDTRY.
          WHEN 106.
            TRY .
                wa_zfit0083-all_in_rate_far   = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-all_in_rate_far   = 0.
            ENDTRY.
          WHEN 107.
            TRY .
                wa_zfit0083-far_leg_forward = t_split-valor.
              CATCH  cx_sy_conversion_overflow.
                wa_zfit0083-far_leg_forward = 0.
            ENDTRY.
          WHEN 108.
            wa_zfit0083-usi_uti_namespa   = t_split-valor.
          WHEN 109.
            wa_zfit0083-usi_uti_identif   = t_split-valor.
          WHEN 110.
            wa_zfit0083-usi_uti_ident_ne  = t_split-valor.

        ENDCASE.

      ENDLOOP.

      " Preciso verificar se o registro processado já existe na tabela.

      CLEAR: it_zfit0083,it_zfit0083_proc.

      APPEND wa_zfit0083 TO it_zfit0083.

      IF wa_zfit0083-rev_trade > 0 . " Se for maior que 0 alguma alteração ocorreu.

        " Se TRANS_TYPE = 2 então só ocorreu modificação
        IF  wa_zfit0083-trans_type = '2'.

          " Pego a ultima alteração.
          v_rev_trade = wa_zfit0083-rev_trade - 1.

          SELECT *
            FROM zfit0083
          INTO TABLE it_zfit0083_proc
            FOR ALL ENTRIES IN it_zfit0083
            WHERE source_ref EQ it_zfit0083-source_ref
            AND rev_trade = v_rev_trade.


          LOOP AT it_zfit0083_proc INTO wa_zfit0083_proc.

            wa_zfit0083_proc-counter_amount  = wa_zfit0083_proc-counter_amount * -1 .
            wa_zfit0083_proc-amount_dealt    = wa_zfit0083_proc-amount_dealt * -1 .
            CONCATENATE 'E' v_rev_trade INTO wa_zfit0083_proc-rev_trade.
            MODIFY zfit0083 FROM wa_zfit0083_proc.
            COMMIT WORK.
            CLEAR wa_zfit0083_proc.


          ENDLOOP.
          " Se TRANS_TYPE = 1 somente estorna pq foi cancelamento
        ELSEIF wa_zfit0083-trans_type = '1' .

          " Pego a ultima alteração.
          v_rev_trade = wa_zfit0083-rev_trade - 1.

          wa_zfit0083-counter_amount  = wa_zfit0083-counter_amount * -1 .
          wa_zfit0083-amount_dealt    = wa_zfit0083-amount_dealt   * -1 .
          CONCATENATE 'E' v_rev_trade INTO wa_zfit0083-rev_trade.

        ENDIF.

      ENDIF.

      " Fim verificação

      MODIFY zfit0083 FROM wa_zfit0083.
      COMMIT WORK.
      CLEAR wa_zfit0083.

      "MOVER PARA OUTRA PASTA
      OPEN DATASET lv_nome_arquivo_proc FOR OUTPUT IN TEXT MODE ENCODING DEFAULT. "NON-UNICODE. "WITH WINDOWS LINEFEED.
      LOOP AT t_file.
        TRANSFER t_file TO lv_nome_arquivo_proc.
      ENDLOOP.

      CLOSE DATASET lv_nome_arquivo_proc.

      DELETE DATASET lv_nome_arquivo.
      CLEAR : t_file[].

    ENDLOOP.
  ENDLOOP.
ENDIF.
