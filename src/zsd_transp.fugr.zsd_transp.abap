FUNCTION zsd_transp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(P_AGENTEF) TYPE  LIFNR OPTIONAL
*"     VALUE(P_REMESSSA) TYPE  VBELN_VL OPTIONAL
*"     VALUE(P_OV) TYPE  VBELN_VA OPTIONAL
*"     VALUE(P_TIPO) TYPE  ZZAUART OPTIONAL
*"  EXPORTING
*"     VALUE(P_TKNUM) TYPE  VTTK-TKNUM
*"     VALUE(T_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA vl_shtyp TYPE shtyp.

  REFRESH t_return.
  CLEAR p_tknum.

  SELECT SINGLE shtyp
    FROM zsdt0011
    INTO vl_shtyp
  WHERE  auart EQ p_tipo.

  IF NOT sy-subrc IS INITIAL.
*   Monta Erro
    PERFORM z_monta_erro TABLES t_return
                          USING text-001
                                p_tipo
                                text-002.
    EXIT.
  ENDIF.

* Preenche Header
  PERFORM: z_preenche_header USING p_werks
                                   p_agentef
                                   vl_shtyp,
* Preenche Item
           z_preenche_item   USING p_remesssa,
* Preenche Stage
           z_preenche_stage  USING p_ov
                                   p_remesssa
                                   p_agentef
                                   p_tipo.

  CALL FUNCTION 'BAPI_SHIPMENT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headerdata = s_header
    IMPORTING
      transport  = p_tknum
    TABLES
      itemdata   = t_item
      stagedata  = t_stage
      return     = t_return.

  IF NOT p_tknum IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFUNCTION.
