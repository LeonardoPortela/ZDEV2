FUNCTION z_sol_baixa_imob_executa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) TYPE  SY-UNAME
*"     VALUE(V_ZUGDT) TYPE  DZUGDAT OPTIONAL
*"     VALUE(V_MOTIVO) TYPE  CHAR30 OPTIONAL
*"     VALUE(V_OBS) TYPE  CHAR72 OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_SOLICITACOES STRUCTURE  ZAAS0001
*"      T_ESTRA STRUCTURE  ZAAS0002
*"----------------------------------------------------------------------

  TYPE-POOLS: icon.

  DATA: BEGIN OF it_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(2) Módulo de função Z_SOL_BAIXA_IMOB_EXECUTA, Estrut. IT_MSG, Fim                                                                                    S
  DATA: END OF it_msg.

  DATA: wg_zaa008    TYPE zaa008,
        e_row_id     TYPE sy-tabix,
        linha_estra  TYPE sy-tabix,
        ult_linha    TYPE sy-tabix,
        wl_cont      TYPE sy-tabix,
        flag_undo(1), "Se 'S', habilitar ícone 'Desfazer'.
        opt          TYPE ctu_params,
        wl_message   TYPE pmst_raw_message.

  "SHDB - ABAVN
  DATA: it_dta TYPE STANDARD TABLE OF bdcdata,
        wa_dta TYPE bdcdata.

  SORT t_estra[] BY nivel.

  LOOP AT t_estra[] INTO DATA(wl_estra) WHERE aprovador EQ v_usuario.

    DATA(vl_check_nivel) = wl_estra-nivel + 1.
    READ TABLE t_solicitacoes INTO DATA(wl_sol) INDEX 1.

*----------------------"Valida se solicitação já foi aprovada por nível superior.
    SELECT SINGLE *
      FROM zaa008
      INTO @DATA(wl_check_log08)
      WHERE anln1 EQ @wl_sol-imobilizado
        AND nivel EQ @vl_check_nivel
        AND dt_estorno IS NULL.

    IF ( sy-subrc EQ 0 ).
      MESSAGE 'Solicitação já aprovada em nível superior!' TYPE 'I'.
      EXIT.
    ENDIF.
*----------------------

    IF ( e_row_id IS NOT INITIAL ) AND ( sy-tabix NE e_row_id + 1 ) .
      EXIT.
    ELSEIF ( e_row_id IS NOT INITIAL ) AND ( sy-tabix EQ e_row_id + 1 ).
      IF ( wl_estra-opcoes NE icon_system_undo ).
        MOVE icon_set_state TO wl_estra-opcoes.
      ENDIF.
      e_row_id = sy-tabix.
    ELSE.
      e_row_id = sy-tabix.
    ENDIF.

    IF ( wl_estra-opcoes NE icon_set_state )   AND
       ( wl_estra-opcoes NE icon_system_undo ) AND
       ( wl_estra-opcoes NE icon_reject ).

      msg =  'Opção inválida para processamento!'.
      EXIT.

    ENDIF.

    IF ( v_usuario NE wl_estra-aprovador ).
      msg =  'Usuário não é o aprovador deste nível!'.
      EXIT.
    ENDIF.

    IF ( wl_estra-opcoes EQ icon_set_state ).
      flag_undo   = 'S'.
      linha_estra = e_row_id.

      LOOP AT t_estra INTO DATA(wl_estra_aux).
        ult_linha = sy-tabix.
        IF ( wl_estra_aux-opcoes EQ icon_set_state ) AND ( sy-tabix LT e_row_id ).
          flag_undo = 'N'.
        ENDIF.
      ENDLOOP.

      IF ( flag_undo EQ 'S' ).

        SELECT * FROM zaa004
        INTO TABLE @DATA(tl_estrat)
        WHERE bukrs EQ @wl_estra-bukrs
          AND gsber EQ @wl_estra-gsber
          AND kostl EQ @wl_estra-kostl
          AND fluxo_baixa EQ 'X'.

        SORT tl_estrat[] BY nivel_aa DESCENDING.
        READ TABLE tl_estrat[] INTO DATA(wl_estrat) INDEX 2.
        DATA(vl_nivel) = wl_estrat-nivel_aa.

        IF ( wl_estra-nivel EQ vl_nivel ).

          UPDATE zaa007
            SET zugdt      = v_zugdt
                motivo     = v_motivo
            WHERE bukrs EQ wl_estra-bukrs
              AND werks EQ wl_estra-gsber
              AND anln1 EQ wl_estra-anln1
              AND kostl EQ wl_estra-kostl.

          COMMIT WORK.
        ENDIF.

        READ TABLE tl_estrat[] INTO wl_estrat INDEX 1.
        vl_nivel = wl_estrat-nivel_aa.

        CLEAR: wl_message.
        IF ( wl_estra-nivel EQ vl_nivel ).

          UPDATE zaa007
            SET status         = 'A'
                obs_controller = v_obs
            WHERE bukrs EQ wl_estra-bukrs
              AND werks EQ wl_estra-gsber
              AND anln1 EQ wl_estra-anln1
              AND kostl EQ wl_estra-kostl.

          COMMIT WORK.

          DATA(vl_dia)    = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum+0(4) }|.
          DATA(vl_mes)    = |{ sy-datum+4(2) }|.

          SELECT SINGLE *
            FROM zaa007
            INTO @DATA(wl_007)
            WHERE bukrs EQ @wl_estra-bukrs
              AND werks EQ @wl_estra-gsber
              AND anln1 EQ @wl_estra-anln1
              AND kostl EQ @wl_estra-kostl.

          IF ( sy-subrc EQ 0 ).
            v_motivo = wl_007-motivo.


**********************************************************************PSA
            DATA: lr_return TYPE  bapiret2.
            CALL FUNCTION 'Z_INATIVA_IMOBILIZADO'
              EXPORTING
                i_bukrs  = wl_estra-bukrs
                i_anln1  = wl_estra-anln1
                i_anln2  = wl_estra-anln2
                i_date   = sy-datum
              IMPORTING
                e_return = lr_return.

            IF lr_return-type = 'E'.
              data msg_e type message-MSGTX.
              msg_e = lr_return-message && ',Favor entrar em contato com a Equipe de Imobilizado!'.
              MESSAGE msg_e TYPE 'I'.
              EXIT.
            ENDIF.

**********************************************************************



*            DATA:gly_new_t   TYPE TABLE OF bapiparex,
*                 gly_new_l   TYPE bapiparex,
*                 gt_oreturn1 TYPE STANDARD TABLE OF bapiret2,
*                 gs_oreturn1 TYPE                   bapiret2,
*                 extension_l TYPE bapi_te_anlu.
*            CLEAR:gly_new_t,gly_new_l.
*            gly_new_l-structure = 'BAPI_TE_ANLU'.
*            extension_l-comp_code = wa_anla-bukrs.
*            extension_l-assetmaino = wa_anla-anln1.
*            extension_l-assetsubno = wa_anla-anln2.
*            gly_new_l-valuepart1 = extension_l  .
*            APPEND gly_new_l TO gly_new_t.
*
*            CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
*              EXPORTING
*                companycode                = wa_anla-bukrs
*                asset                      = wa_anla-anln1
*                subnumber                  = wa_anla-anln2
**               GROUPASSET                 =
**               GENERALDATA                =
**               GENERALDATAX               =
**               INVENTORY                  =
**               INVENTORYX                 =
*                POSTINGINFORMATION         = l_POSTINGINFORMATION
**               POSTINGINFORMATIONX        =
**               TIMEDEPENDENTDATA          =
**               TIMEDEPENDENTDATAX         =
**               ALLOCATIONS                =
**               ALLOCATIONSX               =
**               ORIGIN                     =
**               ORIGINX                    =
**               INVESTACCTASSIGNMNT        =
**               INVESTACCTASSIGNMNTX       =
**               NETWORTHVALUATION          =
**               NETWORTHVALUATIONX         =
**               REALESTATE                 =
**               REALESTATEX                =
**               INSURANCE                  =
**               INSURANCEX                 =
**               LEASING                    =
**               LEASINGX                   =
**               GLO_RUS_GEN                =
**               GLO_RUS_GENX               =
**               GLO_RUS_TRC                =
**               GLO_RUS_TRCX               =
**               GLO_RUS_PTX                =
**               GLO_RUS_PTXX               =
**               GLO_RUS_TTX                =
**               GLO_RUS_TTXX               =
**               GLO_RUS_LTX                =
**               GLO_RUS_LTXX               =
**               GLO_IN_GEN                 =
**               GLO_IN_GENX                =
**               GLO_JP_ANN16               =
**               GLO_JP_ANN16X              =
**               GLO_JP_PTX                 =
**               GLO_JP_PTXX                =
**               GLO_TIME_DEP               =
**               GLO_RUS_GENTD              =
**               GLO_RUS_GENTDX             =
**               GLO_RUS_PTXTD              =
**               GLO_RUS_PTXTDX             =
**               GLO_RUS_TTXTD              =
**               GLO_RUS_TTXTDX             =
**               GLO_RUS_LTXTD              =
**               GLO_RUS_LTXTDX             =
**               GLO_JP_IMPTD               =
**               GLO_JP_IMPTDX              =
**               GLO_KR_BUS_PLACE           =
**               GLO_KR_BUS_PLACEX          =
**               GLO_NATL_CLFN_CODE         =
**               GLO_NATL_CLFN_CODEX        =
**               GLO_PT_FSCL_MAPS           =
**               GLO_PT_FSCL_MAPSX          =
**             IMPORTING
*               RETURN                     = gs_oreturn1
**             TABLES
**               DEPRECIATIONAREAS          =
**               DEPRECIATIONAREASX         =
**               INVESTMENT_SUPPORT         =
*               EXTENSIONIN                = gly_new_t
*                      .
*
*            IF sy-subrc = 0.
*              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                EXPORTING
*                  wait = 'X'.
*            ELSE.
*            ENDIF.

          ENDIF.


***********************************************************************"SHDB - ABAVN
*          "SHDB - ABAVN
*          CLEAR: it_dta[], wa_dta.
*
*          DEFINE shdb.
*            WA_DTA-PROGRAM   = &1.
*            WA_DTA-DYNPRO    = &2.
*            WA_DTA-DYNBEGIN  = &3.
*            WA_DTA-FNAM      = &4.
*            WA_DTA-FVAL      = &5.
*            APPEND WA_DTA TO IT_DTA.
*          END-OF-DEFINITION.
*          shdb:
*          ''          ''      'T'   'ABAVN'             '',
*          'SAPLSPO4'  '0300'  'X'   ''                  '',
*          ''          ''      ''    'BDC_CURSOR'        'SVALD-VALUE(01)',
*          ''          ''      ''    'BDC_OKCODE'        '=FURT',
*          ''          ''      ''    'SVALD-VALUE(01)'   wl_estra-bukrs.
*
*          opt-dismode = 'N'.
*          CALL TRANSACTION 'ABAVN' USING it_dta
*                MODE opt-dismode.
*
*
*          CLEAR: it_dta[], wa_dta.
*          shdb:
*          ''          ''      'T'   'ABAVN'             '',
*          'SAPLAMDP'  '0100'  'X'   ''                  '',
*          ''          ''      ''    'BDC_OKCODE'        '/ENEWC',
*          'SAPLSPO4'  '0300'  'X'   ''                  '',
*          ''          ''      ''    'BDC_CURSOR'        'SVALD-VALUE(01)',
*          ''          ''      ''    'BDC_OKCODE'        '=FURT',
*          ''          ''      ''    'SVALD-VALUE(01)'   wl_estra-bukrs,
*          'SAPLAMDP'  '0100'  'X'   ''                  '',
*          ''          ''      ''    'BDC_OKCODE'        '=TAB02',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0300OBJECT',
*          ''          ''      ''    'RAIFP2-ANLN1'      wl_estra-anln1,
*          ''          ''      ''    'RAIFP2-ANLN2'      wl_estra-anln2,
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0100TABSTRIP',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0200SUBSC',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0300AREA1',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0501AREA2',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0200SUBSCREEN1',
*          ''          ''      ''    'RAIFP1-BLDAT'      vl_dia,
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0201SUBSCREEN2',
*          ''          ''      ''    'RAIFP1-BUDAT'      vl_dia,
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0202SUBSCREEN3',
*          ''          ''      ''    'RAIFP1-BZDAT'      vl_dia,
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN4',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN5',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN6',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN7',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0506AREA3',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0206SUBSCREEN1',
*          ''          ''      ''    'BDC_CURSOR'        'RAIFP2-SGTXT',
*          ''          ''      ''    'RAIFP2-SGTXT'      v_motivo,
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0221SUBSCREEN2',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN3',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN4',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN5',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN6',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLAMDP                                0999SUBSCREEN7',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0300AREA4',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0300AREA5',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0300AREA6',
*          ''          ''      ''    'BDC_SUBSCR'        'SAPLATAB                                0300AREA7',
*          'SAPLAMDP'  '0100'  'X'   ''                  '',
*          ''          ''      ''    'BDC_OKCODE'       '=TAB03',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0300OBJECT',
*          ''          ''      ''    'RAIFP2-ANLN1'     wl_estra-anln1,
*          ''          ''      ''    'RAIFP2-ANLN2'     wl_estra-anln2,
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0100TABSTRIP',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0200SUBSC',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0507AREA1',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0203SUBSCREEN1',
*          ''          ''      ''    'RAIFP2-MONAT'     vl_mes,
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0204SUBSCREEN2',
*          ''          ''      ''    'BDC_CURSOR'       'RAIFP1-BLART',
*          ''          ''      ''    'RAIFP1-BLART'     'AA',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN3',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN4',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN5',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN6',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN7',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0508AREA2',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0205SUBSCREEN1',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN2',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0219SUBSCREEN3',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0220SUBSCREEN4',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN5',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN6',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN7',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0509AREA3',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0207SUBSCREEN1',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0208SUBSCREEN2',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN3',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN4',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN5',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN6',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0999SUBSCREEN7',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0300AREA4',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0300AREA5',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0300AREA6',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0300AREA7',
*          'SAPLAMDP'  '0100'  'X'   ''                 '',
*          ''          ''      ''    'BDC_OKCODE'       '=SAVE',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                              0300OBJECT',
*          ''          ''      ''    'RAIFP2-ANLN1'     wl_estra-anln1,
*          ''          ''      ''    'RAIFP2-ANLN2'     wl_estra-anln2,
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0100TABSTRIP',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLATAB                                0201SUBSC',
*          ''          ''      ''    'BDC_SUBSCR'       'SAPLAMDP                                0401AREA1',
*          ''          ''      ''    'BDC_CURSOR'       'RAIFP2-PROZS',
*          ''          ''      ''    'RAIFP2-PROZS'     '100',
*          ''          ''      ''    'RAIFP2-XAALT'     'X'.
*
*          opt-dismode = 'N'.
*
*          CALL TRANSACTION 'ABAVN' USING it_dta
*                MODE opt-dismode
*                MESSAGES INTO it_msg.
**                UPDATE 'S'.
***********************************************************************"SHDB - ABAVN
          CLEAR wl_cont.
          LOOP AT it_msg WHERE msgtyp EQ 'E'.
            ADD 1 TO wl_cont.
          ENDLOOP.
          IF wl_cont  GT 0.
            CLEAR wl_cont.
            LOOP AT it_msg WHERE msgtyp EQ 'E'.
              ADD 1 TO wl_cont.
              CLEAR: wl_message.
              CALL FUNCTION 'CUTC_GET_MESSAGE'
                EXPORTING
                  msg_type       = it_msg-msgtyp
                  msg_id         = it_msg-msgid
                  msg_no         = sy-msgno
                  msg_arg1       = sy-msgv1
                  msg_arg2       = sy-msgv2
                  msg_arg3       = sy-msgv3
                  msg_arg4       = sy-msgv4
                IMPORTING
                  raw_message    = wl_message
                EXCEPTIONS
                  msg_not_found  = 1
                  internal_error = 2
                  OTHERS         = 3.

              IF ( sy-subrc NE 0 ).
              ENDIF.

              CONCATENATE sy-msgid sy-msgno wl_message INTO wl_message SEPARATED BY space.
              MESSAGE wl_message TYPE 'I'.
            ENDLOOP.
            "
            UPDATE zaa007
              SET status         = ' ' "cancela aprovação
                  obs_controller = v_obs
              WHERE bukrs EQ wl_estra-bukrs
                AND werks EQ wl_estra-gsber
                AND anln1 EQ wl_estra-anln1
                AND kostl EQ wl_estra-kostl.

            COMMIT WORK.
            "

          ENDIF.
        ENDIF.

        MOVE wl_estra-bukrs       TO wg_zaa008-bukrs.
        MOVE wl_estra-gsber       TO wg_zaa008-werks.
        MOVE wl_estra-anln1       TO wg_zaa008-anln1.
        MOVE wl_estra-anln2       TO wg_zaa008-anln2.
        MOVE wl_estra-aprovador   TO wg_zaa008-aprovador.
        MOVE wl_estra-nivel       TO wg_zaa008-nivel.
        MOVE wl_message           TO wg_zaa008-message.
        MOVE sy-datum             TO wg_zaa008-data_atual.
        MOVE sy-uzeit             TO wg_zaa008-hora_atual.

        MODIFY zaa008 FROM wg_zaa008.
        COMMIT WORK.
        CLEAR: wg_zaa008.

        IF ( ult_linha EQ linha_estra ) AND  wl_cont = 0.

          wl_estra-opcoes = icon_system_undo.
          wl_estra-estado = icon_checked.
          MODIFY t_estra[] FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.

        ELSE.

          wl_estra-opcoes = icon_system_undo.
          wl_estra-estado = icon_checked.
          MODIFY t_estra[] FROM wl_estra INDEX e_row_id.
          msg = 'Processamento concluído com sucesso'.

        ENDIF.

      ELSE. "FLAG_UNDO EQ 'S'

        msg = 'Devem ser aprovadas as estratégias anteriores'.

      ENDIF.

    ELSEIF ( wl_estra-opcoes EQ icon_system_undo ).
      flag_undo   = 'S'.
      linha_estra = e_row_id.

      LOOP AT t_estra INTO wl_estra_aux.

        IF ( wl_estra_aux-opcoes EQ icon_system_undo )      AND
           ( wl_estra_aux-aprovador NE wl_estra-aprovador ) AND
           ( sy-tabix GT e_row_id ).

          flag_undo = 'N'.

        ENDIF.

        IF ( wl_estra_aux-opcoes EQ icon_message_critical ).
          msg = 'Solicitação totalmente liberada'.
          flag_undo = 'N'.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF ( flag_undo EQ 'S' ).

        DELETE FROM zaa008
          WHERE bukrs     EQ wl_estra-bukrs
            AND werks     EQ wl_estra-gsber
            AND anln1     EQ wl_estra-anln1
            AND nivel     EQ wl_estra-nivel
            AND aprovador EQ wl_estra-aprovador.

        wl_estra-estado = icon_led_yellow.
        wl_estra-opcoes = icon_set_state.
        MODIFY t_estra FROM wl_estra INDEX e_row_id.
      ELSE.
        msg = 'Devem ser reiniciadas as estratégias posteriores'.
      ENDIF.

    ELSEIF ( wl_estra-opcoes EQ icon_reject ).

      UPDATE zaa007
        SET status      = 'R'
            data_atual  = sy-datum
            obs_controller = v_obs
        WHERE bukrs EQ wl_estra-bukrs
          AND werks EQ wl_estra-gsber
          AND anln1 EQ wl_estra-anln1.
*          AND ANLN2 EQ WL_ESTRA-ANLN2.

      UPDATE zaa008
        SET status        = 'R'
            data_atual    = sy-datum
            dt_estorno    = sy-datum
            hr_estorno    = sy-uzeit
            user_estorno  = sy-uname
        WHERE bukrs EQ wl_estra-bukrs
          AND werks EQ wl_estra-gsber
          AND anln1 EQ wl_estra-anln1
          AND anln2 EQ wl_estra-anln2.

      COMMIT WORK.

    ENDIF.

    MODIFY t_estra FROM wl_estra.

  ENDLOOP.

ENDFUNCTION.
