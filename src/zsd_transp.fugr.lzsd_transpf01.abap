*----------------------------------------------------------------------*
***INCLUDE LZSD_TRANSPF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_HEADER                                        *
*&---------------------------------------------------------------------*
*                             Preenche Header                          *
*----------------------------------------------------------------------*
FORM z_preenche_header USING p_werks   TYPE werks_d
                             p_agentef TYPE lifnr
                             p_shtyp   TYPE shtyp.
  CLEAR s_header.

  s_header-shipment_type        = p_shtyp.
  s_header-trans_plan_pt        = p_werks.
  s_header-service_level        = 1.
  s_header-shipping_type        = '01'.
*  s_header-external_id_1        = p_fatura.
  s_header-status_plan          = 'X'.
*  s_header-status_checkin       = 'X'.
*  s_header-status_load_start    = 'X'.
*  s_header-status_load_end      = 'X'.
*  s_header-status_compl         = 'X'.
*  s_header-status_shpmnt_start  = 'X'.
*  s_header-status_shpmnt_end    = 'X'.
  s_header-service_agent_id     = p_agentef.
  s_header-time_travel          = 30.
  s_header-time_total           = 1.
  s_header-time_unit            = 'H'.
  s_header-special_procedure_id = '0001'.
  s_header-shpmnt_cost_rel      = 'X'.

ENDFORM.                    " Z_PREENCHE_HEADER

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_ITEM                                          *
*&---------------------------------------------------------------------*
*                               Preenche Item                          *
*----------------------------------------------------------------------*
FORM z_preenche_item USING p_remesssa TYPE vbeln_vl.

  DATA sl_item TYPE bapishipmentitem.

  REFRESH t_item.

  sl_item-delivery  = p_remesssa.
  sl_item-itenerary = 10.

  APPEND sl_item TO t_item.

ENDFORM.                    " Z_PREENCHE_ITEM

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_STAGE                                         *
*&---------------------------------------------------------------------*
*                             Preenche Stage                           *
*----------------------------------------------------------------------*
FORM z_preenche_stage USING p_ov      TYPE vbeln_va
                            p_remessa TYPE vbeln_vl
                            p_afrete  TYPE lifnr
                            p_tipo    TYPE zzauart.

  DATA: tl_vbpa    TYPE TABLE OF vbpa    ,
        sl_stage   TYPE bapishipmentstage,
        sl_vbpa_lr TYPE vbpa             ,
        sl_vbpa_pc TYPE vbpa             .

  REFRESH t_stage.

  CALL FUNCTION 'SD_VBPA_READ_WITH_VBELN'
    EXPORTING
      i_vbeln          = p_remessa
    TABLES
      et_vbpa          = tl_vbpa
    EXCEPTIONS
      record_not_found = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  SORT tl_vbpa BY parvw ASCENDING.
  DELETE tl_vbpa WHERE parvw NE 'LR'
                   AND parvw NE 'PC'.

  READ TABLE tl_vbpa INTO sl_vbpa_lr
    WITH KEY parvw = 'LR'
    BINARY SEARCH.

  READ TABLE tl_vbpa INTO sl_vbpa_pc
   WITH KEY parvw = 'PC'
   BINARY SEARCH.

  sl_stage-stage_cat     = '1'.
  sl_stage-stage_seq     = '0001'.
  sl_stage-shipping_type = '01'.
  sl_stage-leg_indicator = '4'.
  sl_stage-service_agent = p_afrete.
  "Para transferencia entre plantas deve ser passado como local de expedição, senão duplica a etapa
  IF p_tipo EQ 'ZUB'.
    sl_stage-org_shipp_dpmnt = sl_vbpa_lr-kunnr+06(04).
  ELSE.
    sl_stage-dest_cust     = sl_vbpa_lr-kunnr.
  ENDIF.
  sl_stage-org_suppl     = sl_vbpa_pc-lifnr.

  APPEND sl_stage TO t_stage.

ENDFORM.                    " Z_PREENCHE_STAGE

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                               Monta Erro                             *
*----------------------------------------------------------------------*
FORM z_monta_erro TABLES  p_return  STRUCTURE bapiret2
                   USING  p_text001 TYPE c
                          p_tipo    TYPE zzauart
                          p_text002 TYPE c.

  DATA sl_ret TYPE bapiret2.

  sl_ret-type = 'E'.
  CONCATENATE p_text001
              p_tipo
              p_text002
         INTO sl_ret-message SEPARATED BY space.

  APPEND sl_ret TO p_return.

ENDFORM.                    " Z_MONTA_ERRO
