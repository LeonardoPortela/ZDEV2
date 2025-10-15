*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 18.01.2022                                              *
* Descrição: Ajustar OV Dummy
* Report   : VA02                                                    *
*--------------------------------------------------------------------*
* Projeto  : CS2021001045                                            *
*--------------------------------------------------------------------*

DATA: l_time TYPE timestampl,
      l_task TYPE char40,
      l_dest TYPE char10.

GET TIME STAMP FIELD l_time.

l_task = l_time.
l_dest = 'NONE'.

CHECK sy-tcode = 'VA02'.

CHECK t_xvbap[] IS NOT INITIAL.

CALL FUNCTION 'ZSD_AJUSTA_OV_DUMMY'
  DESTINATION l_dest
  STARTING NEW TASK l_task
  EXPORTING
    i_vbeln_venda = xvbak-vbeln
  TABLES
    t_xvbap       = t_xvbap.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*

.
