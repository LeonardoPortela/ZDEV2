"Name: \PR:SAPMIEQ0\FO:LEAVE_TO_TRANSACTION\SE:BEGIN\EI
ENHANCEMENT 0 ZZ_CRIAR_ORDEM_PM_IE01.

TRY.
IF sy-ucomm EQ 'BU'.
IF 'IE01_IE31_IE10' CS sy-tcode.
      IF EQUI-eqtyp EQ '1'
      OR EQUI-eqtyp EQ '2'
      OR EQUI-eqtyp EQ '3'
      OR EQUI-eqtyp EQ '4'.
"Automatizar processo para criar ordens / ponto medição / transf.ponto / lançamento inicial para ponto / plantos de manutenção.
ZCL_PM_DATA_EQUIPAMENT=>zif_PM_DATA_EQUIPAMENT~get_instance(
    )->set_pos_contador( i_pos_contador = fleet-zzhor_odom_inicial
    )->SET_DATA_EQPTO( I_EQUI = EQUI I_EQUZ = EQUZ I_ILOA = ILOA I_ITOBATTR = ITOBATTR i_fleet = fleet ).

ENDIF.
ENDIF.
ENDIF.
CATCH zcx_error INTO DATA(ex_error).
ENDTRY.

ENDENHANCEMENT.
