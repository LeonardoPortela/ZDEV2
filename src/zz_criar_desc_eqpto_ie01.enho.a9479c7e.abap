"Name: \PR:SAPMIEQ0\FO:ITOB_TRANSFER\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CRIAR_DESC_EQPTO_IE01.

*-US 158036-04-12-2024-#158036-RJF-Inicio
IF  'IE02_IE01_IE31' CS sy-tcode.
  IF rm63e-equnr IS NOT INITIAL.
    BREAK rfreitas.
    DATA: ln_kostln(4)  TYPE n,
          ln_kostlm(10) TYPE n.

    SELECT equnr, eqtyp, eqart, kostl
      UP TO 1 ROWS
      FROM itob " *ITOB-EQTYP - Objetos técnicos PM (EQUI, local de instalação)
      INTO @DATA(wa_itob)
      WHERE equnr EQ @rm63e-equnr.
    ENDSELECT.
    IF sy-subrc IS INITIAL.

      ln_kostlm = itob-kostl.

      IF  wa_itob-eqtyp EQ '1'
       OR wa_itob-eqtyp EQ '2'
       OR wa_itob-eqtyp EQ '3'
       OR wa_itob-eqtyp EQ '4'
       OR wa_itob-eqtyp EQ 'A'.

        SELECT * FROM zpmt0001
        INTO TABLE @DATA(it_zpmt0001)
        WHERE eqtyp  EQ @wa_itob-eqtyp
          AND eqart  EQ @wa_itob-eqart.

        IF sy-subrc IS INITIAL.
          SORT it_zpmt0001 BY eqtyp eqart.
          LOOP AT it_zpmt0001 INTO DATA(wa_pmt0001) WHERE eqtyp = wa_itob-eqtyp
                                                      AND eqart = wa_itob-eqart.
            ln_kostln  = wa_pmt0001-kostlg.
            IF sy-subrc IS INITIAL AND ln_kostln IS NOT INITIAL.
              FREE itob-kostl.
              ln_kostlm = iloa-bukrs+2(2) && '0' && iloa-gsber+2(2) && ln_kostln.
              iloa-kostl = ln_kostlm.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
*-US 158036-04-12-2024-#158036-RJF-Fim

**=================USER STORY 96090  Anderson Oenning
IF  'IE02_IE01_IE31' CS sy-tcode.
  "Automatizar descrição do veiculo.
  TRY .
      IF rm63e-eqtyp   EQ '1'
        OR rm63e-eqtyp EQ '2'
        OR rm63e-eqtyp EQ '3'
        OR rm63e-eqtyp EQ '4'.


        zcl_pm_data_equipament=>zif_pm_data_equipament~get_instance(
        )->set_eartx( i_eqart =  equi-eqart
        )->set_desc_fabricante( i_herst =  equi-herst
        )->set_desc_modelo( i_typbz =  equi-typbz
        )->set_desc_eqpto( IMPORTING e_ktx01 = eqkt-eqktx ).

        IF eqkt-eqktx IS NOT INITIAL.
          itob-shtxt = eqkt-eqktx.
        ENDIF.
      ENDIF.
    CATCH zcx_error INTO DATA(ex_erro).

  ENDTRY.
**********************************************************************************************
**Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - INICIO
**********************************************************************************************
  DATA: v_texto       TYPE ktx01,
        v_imobiizado  TYPE anln1,
        v_equipamento TYPE equnr,
        v_kostl       TYPE kostl,
        v_gsber       TYPE gsber,
        v_bukrs       TYPE bukrs.

  CLEAR: v_texto.

  v_texto = eqkt-eqktx(25).
*  IF ITOB-EQUNR IS NOT INITIAL or FLEET-OBJNR IS NOT INITIAL.
*    IF ITOB-ANLNR IS NOT INITIAL .
*      V_IMOBIIZADO = ITOB-ANLNR.
*      v_equipamento = ITOB-EQUNR.
*      V_KOSTL = ITOB-KOSTL.
*      V_TEXTO = ITOB-SHTXT(25).
*      V_BUKRS = ITOB-BUKRS.
*      V_GSBER = ITOB-GSBER.
*
*    ELSEIF FLEET-ZZIMOBILIZADO IS NOT INITIAL .
*      V_IMOBIIZADO = FLEET-ZZIMOBILIZADO.
*      v_equipamento = EQUI-EQUNR.
*      V_KOSTL = iloa-KOSTL.
*      V_BUKRS = iloa-BUKRS.
*      v_GSBER = iloa-GSBER.
*    ENDIF.
*  ENDIF.


*  IF iloa-anlnr IS INITIAL.
*     V_IMOBIIZADO = FLEET-ZZIMOBILIZADO.
*      ELSE.
*         V_IMOBIIZADO =   iloa-anlnr.
*    ENDIF.
*    CALL FUNCTION 'Z_TRANSFERIR_IMOBILIZADO'
*        EXPORTING
*          IMOBILIZADO = V_IMOBIIZADO
*          EQUNR       = EQUI-EQUNR "v_equipamento
*          KOSTL       = iloa-KOSTL "v_KOSTL
*          SHTXT       = V_TEXTO
*          BUKRS       = iloa-BUKRS "v_BUKRS
*          GSBER       = iloa-GSBER."v_GSBER.
***********************************************************************************************
***Criar camp D. MESTRES do AA para sinc. com PM - BG #142094 - FIM
***********************************************************************************************

ENDIF.


"#158521 - Fabricio - inicio
IF  'IE02_IE01_IE31' CS sy-tcode
AND ( wa_itob-eqtyp EQ '1'
   OR wa_itob-eqtyp EQ '2'
   OR wa_itob-eqtyp EQ '3'
   OR wa_itob-eqtyp EQ '4'
   OR wa_itob-eqtyp EQ 'A' ).

  DATA(lo_pm_data_equipament) = NEW zcl_pm_data_equipament( ).

  lo_pm_data_equipament->zif_pm_data_equipament~valida_capacidade_combustivel(
    EXPORTING
      i_cod_classe           = equi-eqart
      i_fabricante           = equi-herst
      i_modelo               = equi-typbz
*    i_qtd_abastec          =
    IMPORTING
      e_tq_comb              = DATA(lv_tq_comb)
      e_dentro_da_tolerancia = DATA(lv_dentro_tolerancia) ).

  IF fleet-key_num IS NOT INITIAL AND
     lv_tq_comb IS NOT INITIAL AND
     fleet-key_num <> lv_tq_comb.

    fleet-key_num = lv_tq_comb.

    MESSAGE s024(sd) WITH |Verificar parâmetros de TQ Abast transação ZPM0017| DISPLAY LIKE 'E'.

  ENDIF.

ENDIF.

"#158521 - Fabricio - fim

ENDENHANCEMENT.
