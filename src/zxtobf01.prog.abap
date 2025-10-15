*----------------------------------------------------------------------*
***INCLUDE ZXTOBF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 24/09/2024  |DEVK9A2791  |NSEGANTIN      |Eqpt Tp 01, Obrig Imobiliz*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form zf_dynpro_fields_2000
*&---------------------------------------------------------------------*
*& Validação de alteração de dados
*&---------------------------------------------------------------------*
FORM zf_dynpro_fields_2000.

  FIELD-SYMBOLS <fs_fleet> TYPE fleet.

  ASSIGN ('(SAPLITO0)FLEET') TO <fs_fleet>.

  CHECK <fs_fleet> IS ASSIGNED.

  <fs_fleet>-div1               = fleet-div1.
  <fs_fleet>-div2               = fleet-div2.
  <fs_fleet>-div3               = fleet-div3.
  <fs_fleet>-tq_combustivel_1   = fleet-tq_combustivel_1.
  <fs_fleet>-tq_combustivel_2   = fleet-tq_combustivel_2.
  <fs_fleet>-tq_combustivel_3   = fleet-tq_combustivel_3.
  <fs_fleet>-zzhor_odom_inicial = fleet-zzhor_odom_inicial.
  <fs_fleet>-zzpot_motor        = fleet-zzpot_motor.
  <fs_fleet>-zzun_motor         = fleet-zzun_motor.
  <fs_fleet>-zzimobilizado      = fleet-zzimobilizado.

ENDFORM.
