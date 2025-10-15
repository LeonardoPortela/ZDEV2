FUNCTION zsdmf_insumos_simula_preco.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN_REF) TYPE  VBELN_VA
*"     REFERENCE(IV_POSNR) TYPE  POSNR_VA
*"     REFERENCE(IV_QTDE) TYPE  KWMENG
*"     REFERENCE(IV_PR00_ATUAL) TYPE  BAPIKBETR1
*"     REFERENCE(IV_RB00_ATUAL) TYPE  BAPIKBETR1
*"     REFERENCE(IV_ICMI_ATUAL) TYPE  BAPIKBETR1
*"  EXPORTING
*"     REFERENCE(EV_PR00_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(EV_RB00_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(EV_INTERACOES) TYPE  SY-TABIX
*"  TABLES
*"      ET_BAPICOND STRUCTURE  BAPICOND OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_pr00_temp TYPE bapikbetr1.
  DATA lv_rb00_temp TYPE bapikbetr1.
  DATA lv_icmi_temp TYPE bapikbetr1.
  DATA lv_icmi_aux TYPE bapikbetr1.
  DATA lv_dife_temp TYPE kbetr.
  DATA lv_icva_kbert TYPE  bapikbetr1.
  DATA lv_icbs_kbert TYPE  bapikbetr1.
  DATA lv_rb00_convert TYPE  kwert.

  CLEAR ev_interacoes.

  DO.

    " Tem diferença no preço?, ou primeira execução
    IF lv_dife_temp <> 0 OR sy-index = 1.

      " se tiver, então assume temporariamente o valor de pr00 para verificar
      lv_pr00_temp = lv_pr00_temp + lv_dife_temp.

      " adiciona 1 no contador de interações
      ADD 1 TO ev_interacoes.

      " se a diferença de preço for zero, então pode sair do loop
    ELSE.
      EXIT.
    ENDIF.

    CLEAR et_bapicond[].

    " para simular, é feito conforme VA02, ou seja é passado o valor temporario de pr00
    " e valor zerado de rb00
    CALL FUNCTION 'ZSDMF_INSUMOS_OV_SIMULATE'
      EXPORTING
        iv_vbeln_ref  = iv_vbeln_ref
        iv_posnr      = iv_posnr
        iv_pr00       = lv_pr00_temp
        iv_rb00       = '0.00'
      IMPORTING
        ev_icmi_cond  = lv_icmi_temp
        ev_icva_kbert = lv_icva_kbert
        ev_icbs_kbert = lv_icbs_kbert
      TABLES
        et_bapicond   = et_bapicond[].

    " verifica diferença entre icmi antigo e o novo
    lv_icmi_aux  = iv_icmi_atual - lv_icmi_temp.

    " divide pela quantidade para pegar fator
    lv_dife_temp = lv_icmi_aux / iv_qtde.

  ENDDO.

  ev_pr00_novo = lv_pr00_temp.

  " calcula o coeficiente
  CALL FUNCTION 'ZSDMF_INSUMOS_COEFICIENTE'
    EXPORTING
      iv_icva_kbert      = CONV vfprc_element_amount( lv_icva_kbert )
      iv_icbs_kbert      = CONV vfprc_element_amount( lv_icbs_kbert )
      iv_kwert_icmi      = CONV vfprc_element_amount( iv_icmi_atual )
      iv_kwert_icmi_novo = CONV vfprc_element_amount( lv_icmi_temp )
    IMPORTING
      ev_rb00_novo       = lv_rb00_convert.

  " sempre inverte o rb00
  ev_rb00_novo = lv_rb00_convert * -1.

ENDFUNCTION.
