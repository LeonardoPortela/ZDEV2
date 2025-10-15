*&--------------------------------------------------------------------------------&*
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 26.05.2025                                                          &*
*& Descrição: Calculo Impostos Bonificacao                                        &*
*&--------------------------------------------------------------------------------&*

FORM f_calcula_impostos TABLES t_tax_data TYPE ty_txdata
                               t_tax_laws TYPE ty_txlaws.

  DATA: lc_impostos_nf  TYPE REF TO zcl_impostos_nf,
        t_impostos      TYPE zfiwrt0010_t,
        lv_leis_fiscais TYPE zmmt0154.

  FIELD-SYMBOLS: <f_mkpf> TYPE mkpf,
                 <f_mseg> TYPE mseg.

  ASSIGN ('(SAPMM07M)XMKPF') TO <f_mkpf>.
  CHECK sy-subrc = 0.

  ASSIGN ('(SAPMM07M)XMSEG') TO <f_mseg>.
  CHECK sy-subrc = 0.

*------------------------------------------
* TVARV
*------------------------------------------
  SELECT SINGLE low
    INTO @DATA(_low)
    FROM tvarvc
   WHERE name = 'MAGGI_TP_MOV_BONIFICACAO'
     AND low  = @<f_mseg>-bwart.

  CHECK sy-subrc = 0.

*------------------------------------------
* calculo dos impostos
*------------------------------------------
  CREATE OBJECT lc_impostos_nf.

  lc_impostos_nf->calcula_impostos_bonificacao( EXPORTING i_chave_nfe    = <f_mkpf>-zchave_nfe
                                                          i_header       = wa_nf_doc
                                                          i_itens        = wa_nf_lin
                                                IMPORTING e_impostos     = t_impostos
                                                          e_leis_fiscais = lv_leis_fiscais ).

  IF t_impostos[] IS NOT INITIAL.
    FREE: t_tax_data.
    LOOP AT t_impostos INTO DATA(_impostos).
      t_tax_data-taxtyp = _impostos-taxtyp.
      t_tax_data-base   = _impostos-base.
      t_tax_data-rate   = _impostos-rate.
      t_tax_data-taxval = _impostos-taxval.
      t_tax_data-othbas = _impostos-othbas.
      APPEND t_tax_data.
    ENDLOOP.
  ENDIF.

  IF lv_leis_fiscais IS NOT INITIAL.
    FREE: t_tax_laws.
    t_tax_laws-kposn    = <f_mseg>-zeile.
    t_tax_laws-icmslaw  = lv_leis_fiscais-j_1btaxlw1.
    t_tax_laws-ipilaw   = lv_leis_fiscais-j_1btaxlw2.
    t_tax_laws-coflaw   = lv_leis_fiscais-j_1btaxlw4.
    t_tax_laws-pislaw   = lv_leis_fiscais-j_1btaxlw5.
    wa_nf_lin-cfop      = lv_leis_fiscais-cfop.
    APPEND t_tax_laws.
  ENDIF.

ENDFORM.
