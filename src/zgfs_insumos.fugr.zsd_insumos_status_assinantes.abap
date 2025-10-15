FUNCTION zsd_insumos_status_assinantes.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ID_DOCUMENTO) TYPE  ZID_DOCUMENTO
*"  EXCEPTIONS
*"      SEM_STATUS
*"----------------------------------------------------------------------

  FREE: l_erro, t_assinantes, t_parti, it_fieldcat.

  gv_id = i_id_documento. " US #158242 - RAMON

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*-----------------------------
* Obter status assinantess
*-----------------------------
  TRY .
      zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
         )->get_obter_status_assinatura( EXPORTING i_id_documento       = i_id_documento
                                         IMPORTING t_status_assinaturas = t_assinantes ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      l_erro = abap_true.
    CATCH zcx_error INTO DATA(ex_error).
      l_erro = abap_true.
  ENDTRY.

  IF l_erro = abap_true.
    SELECT SINGLE *
      FROM zsdt0310
      INTO @DATA(w_0310)
     WHERE id_documento = @i_id_documento.

    IF w_0310-tipo_doc_digital = 'S'.
      RAISE sem_status.
    ENDIF.

    SELECT *
      FROM zsdt0316
      INTO TABLE @DATA(t_0316)
     WHERE nr_doc_gerado    = @w_0310-nr_doc_gerado
       AND id_doc_agrupador = @w_0310-id_documento.

    LOOP AT t_0316                           INTO DATA(w_0316).
      w_assinantes-situacaoassinatura-chave     = 'MANUAL'.
      w_assinantes-nome                         = w_0316-nome.
      w_assinantes-codigo                       = w_0316-codigo.
      w_assinantes-email                        = w_0316-email.
      w_assinantes-situacaoassinatura-descricao = 'Manual'.
      APPEND w_assinantes                      TO t_assinantes.
    ENDLOOP.
  ENDIF.

  IF t_assinantes[] IS INITIAL.
    RAISE sem_status.
  ENDIF.

*-----------------------------
* montar alv
*-----------------------------
  LOOP AT t_assinantes INTO w_assinantes.

    CHECK w_assinantes-situacaoassinatura-chave IS NOT INITIAL.

    CASE w_assinantes-situacaoassinatura-chave(4).
      WHEN 'MANU'.
        w_parti-status = icon_annotation.
      WHEN 'CONC'.
        w_parti-status = icon_agent_uptodate.
      WHEN 'PEND'.
        w_parti-status = icon_agent_outdate.
      WHEN 'AGUA'.
        w_parti-status = icon_time.
      WHEN 'CANC' OR 'REJE'.
        w_parti-status = icon_agent_orphan.
    ENDCASE.

    w_parti-nome       = w_assinantes-nome.
    w_parti-codigo     = w_assinantes-codigo.
    w_parti-email      = w_assinantes-email.
    w_parti-descricao  = w_assinantes-situacaoassinatura-descricao.

    APPEND w_parti    TO t_parti.
  ENDLOOP.

*-----------------------------
* colunas alv
*-----------------------------
  PERFORM f_preenche_fcat USING :
   '01' ''          ''            'T_PARTI'  'STATUS'                 'Status'                 '05'     ''    ''     ''    '' '' 'X',
   '02' ''          ''            'T_PARTI'  'NOME'                   'Nome'                   '30'     ''    ''     ''    '' '' ' ',
   '03' ''          ''            'T_PARTI'  'CODIGO'                 'CPF'                    '15'     ''    ''     ''    '' '' ' ',
   '04' ''          ''            'T_PARTI'  'EMAIL'                  'Email'                  '25'     ''    ''     ''    '' '' ' ',
   '05' ''          ''            'T_PARTI'  'DESCRICAO'              'Status Descr.'          '20'     ''    ''     ''    '' '' ' '.

*-----------------------------
* layout
*-----------------------------
  ls_variant-report = sy-repid && 'XXX'.
  l_grid_title      = 'Status das Assinaturas'.

*-----------------------------
* exibe alvv
*-----------------------------
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      it_fieldcat              = it_fieldcat[]
      i_callback_user_command  = 'USER_COMMAND_COMPRO'
      i_grid_title             = l_grid_title
      i_save                   = 'X'
      is_variant               = ls_variant
      i_screen_start_column    = 40
      i_screen_start_line      = 08
      i_screen_end_column      = 145
      i_screen_end_line        = 18
    TABLES
      t_outtab                 = t_parti.

ENDFUNCTION.

FORM user_command_compro USING ucomm TYPE syucomm
                            selfield TYPE slis_selfield.

  CASE ucomm.

    WHEN 'OK'.

      LEAVE TO SCREEN 0.

    WHEN 'CANC'.

      LEAVE TO SCREEN 0.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
