class ZCL_EMBARQUE definition
  public
  final
  create public .

public section.

  class-data L_CHAR10 type CHAR10 .
  class-data L_CHAR3 type CHAR3 .
  class-data L_CHAR3B type CHAR3 .
  class-data L_NROSOL type ZDE_NRO_SOL . " RIM CS1029457 ANB 30.09.2022
  class-data L_SEQCAM type NUMC3 .
  class-data L_OTHERS type ZDE_AUT_EMBARQUE .
  class-data L_OTHERS_AUX type ZDE_AUT_EMBARQUE .
  class-data L_SEQ type NUMC3 .
  class-data L_FILIAL_RESP type VKBUR .
  class-data L_VKBUR type VKBUR .

  class-methods VALIDAR_AUT_EMBARQUE
    importing
      value(I_AUT_EMBARQUE) type ZDE_AUT_EMBARQUE optional
    exporting
      value(E_SEQ_CAM) type NUMC3
      value(E_NRO_SOL) type ZDE_NRO_SOL
      value(E_SEQ) type NUMC3
      value(E_FILIAL_RESP) type VKBUR
    returning
      value(R_AUT_EMBARQUE) type ZDE_AUT_EMBARQUE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EMBARQUE IMPLEMENTATION.


  METHOD validar_aut_embarque.

    CLEAR: e_nro_sol,
           e_seq,
           e_seq_cam,
           e_filial_resp,
           r_aut_embarque,
           l_vkbur.

    CHECK i_aut_embarque IS NOT INITIAL.

    CONDENSE i_aut_embarque.

    SPLIT i_aut_embarque  AT '/' INTO l_char10 l_others.
    SPLIT l_others        AT '-' INTO l_char3  l_others.
    SPLIT l_others        AT ' ' INTO l_char3b l_filial_resp.

    l_nrosol      = l_char10.
    l_seq         = l_char3.
    l_seqcam      = l_char3b.

    IF l_nrosol      IS INITIAL OR
       l_seq         IS INITIAL OR
       l_seqcam      IS INITIAL OR
       l_filial_resp IS INITIAL.
      EXIT.
    ENDIF.

*---------------------------------
*-- valida escritorio de vendas
*---------------------------------
    SELECT SINGLE vkbur
             INTO l_vkbur
             FROM tvbur
            WHERE vkbur = l_filial_resp.

    CHECK sy-subrc = 0.

*---------------------------------
*-- atribuicao saida
*---------------------------------
    e_seq_cam     = l_seqcam.
    e_nro_sol     = l_nrosol.
    e_seq         = l_seq.
    e_filial_resp = l_filial_resp.

    CONCATENATE l_seqcam l_filial_resp
           INTO l_others_aux
      SEPARATED BY space.

    CONCATENATE l_nrosol '/' l_seq '-' l_others_aux
           INTO r_aut_embarque.

  ENDMETHOD.
ENDCLASS.
