FUNCTION zsdmf_insumos_agrupamento_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(IR_VBELN_RANGE) TYPE  SHP_VBELN_RANGE_T OPTIONAL
*"     REFERENCE(IV_SEM_POPUP) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EV_VBELN_AGP) TYPE  VBELN
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"  TABLES
*"      ET_OVS_AGRUP STRUCTURE  ZSDS084 OPTIONAL
*"      ET_OVS STRUCTURE  ZSDS084 OPTIONAL
*"      ET_0090 STRUCTURE  ZSDT0090 OPTIONAL
*"      ET_0041 STRUCTURE  ZSDT0041 OPTIONAL
*"----------------------------------------------------------------------

  CLEAR et_0090[].BREAK rblima.

  CALL FUNCTION 'ZSDMF_SELECIONA_OV_AGRUPAMENTO'
    EXPORTING
      iv_vbeln       = iv_vbeln
      ir_vbeln_range = ir_vbeln_range
      iv_sem_popup   = iv_sem_popup
    IMPORTING
      ev_canc        = ev_erro
    TABLES
      et_ovs         = et_ovs.

  CHECK ev_erro IS INITIAL.

  PERFORM f_agrupar_ovs CHANGING ev_erro et_ovs[] et_ovs_agrup[].

  IF ev_erro IS INITIAL.

    PERFORM f_gerar_ov USING iv_commit et_ovs_agrup[] CHANGING ev_vbeln_agp ev_erro.

    IF ev_erro IS INITIAL.

      PERFORM f_atualiza_ztabs
        USING ev_vbeln_agp
              iv_commit
              et_ovs_agrup[]
              et_ovs[]
     CHANGING ev_erro
              et_0090[]
              et_0041[].

    ENDIF.

  ENDIF.

ENDFUNCTION.
