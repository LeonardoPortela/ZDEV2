*&---------------------------------------------------------------------*
*&  Include           ZXTOBU04
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 24/09/2024  |DEVK9A2791  |NSEGANTIN      |Eqpt Tp 01, Obrig Imobiliz*
*&---------------------------------------------------------------------*
TABLES fleet.
fleet-div1 = i_data_fleet-div1.
fleet-div2 = i_data_fleet-div2.
fleet-div3 = i_data_fleet-div3.

fleet-tq_combustivel_1 = i_data_fleet-tq_combustivel_1.
fleet-tq_combustivel_2 = i_data_fleet-tq_combustivel_2.
fleet-tq_combustivel_3 = i_data_fleet-tq_combustivel_3.
fleet-zzhor_odom_inicial = i_data_fleet-zzhor_odom_inicial.

fleet-zzpot_motor   = i_data_fleet-zzpot_motor.
fleet-zzun_motor   = i_data_fleet-zzun_motor.
**<<<------"149356 - NMS - INI------>>>
*fleet-zzimobilizado   = i_data_fleet-zzimobilizado.
* Verifica o Tipo do Equipamento.
CASE i_data_equi-eqtyp.
  WHEN '1'.   "1-Agro Próprio
    fleet-zzimobilizado = i_data_fleet-zzimobilizado.

  WHEN '2' OR "2-Agro Locado
       '3' OR "3-Agro Terceiro
       '4'.   "4-Agro Emprestado/Teste
    fleet-zzimobilizado = 'N/A'.

  WHEN OTHERS.
* Do nothing
ENDCASE.
**<<<------"149356 - NMS - FIM------>>>
