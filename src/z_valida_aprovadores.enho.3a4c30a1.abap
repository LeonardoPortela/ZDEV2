"Name: \PR:SAPLKBPP\FO:TRANSFER_BPIF\SE:END\EI
ENHANCEMENT 0 Z_VALIDA_APROVADORES.
* Valida valores de orçamento de acordo com o cadasto de aprovadores (zpm0021)
  if ( sy-tcode = 'KO22' OR sy-tcode = 'KO24' )
  and sy-ucomm  = 'POST'.

    data: lv_valor   type cAUFV-USER4 value 0,
          lv_waers   type caufv-waers,
          LV_AUT_ORC type i,
          LV_AUT_SUP type i,
          LV_AUART   type C LENGTH 4,
          LV_MSG     TYPE C LENGTH 220.

    FIELD-SYMBOLS: <fs_ja> like t_ja.

**  Seleciona tipo de ordem
    select single auart
      into lv_auart
      from caufv
      where aufnr = bpin-IDENT.

    IF lv_auart EQ 'ZPM7'.
**  Soma valores orçados para o anos
      LOOP AT t_ja ASSIGNING <fs_ja> WHERE epos = ''
                                      AND  vorga = 'KBFC'.
        ADD <fs_ja>-WTJHV TO lv_valor.
        lv_waers = <fs_ja>-twaer.
      ENDLOOP.

**   Checa permissão de usuário
      CALL FUNCTION 'Z_APROV_LIBERA_ORDEM_PM'
        EXPORTING
          I_BUKRS        = bpin-BUKRS
          I_WERKS        = bpin-GSBER
          I_USER         = SY-UNAME
          I_TCODE        = SY-TCODE
          I_WAERS        = lv_WAERS
          I_VALOR        = lv_valor
        IMPORTING
          E_AUTORIZA_ORCA = LV_AUT_ORC
          E_AUTORIZA_SUPL = LV_AUT_SUP.

      IF  SY-TCODE = 'KO22'
      AND LV_AUT_ORC IS NOT INITIAL.
        CONCATENATE 'Alteração de orçamento, não permitida procurar gestor nível superior (' BPIN-IDENT ').' INTO LV_MSG.
        MESSAGE LV_MSG TYPE 'E'.
      ELSEIF SY-TCODE = 'KO24'
      AND    LV_AUT_SUP IS NOT INITIAL.
        CONCATENATE 'Suplementação de orçamento, não permitida procurar gestor nível superior (' BPIN-IDENT ').' INTO LV_MSG.
        MESSAGE LV_MSG TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
