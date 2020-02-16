class Z6HR006C_LEAVE_REQ_SCREEN definition
  public
  final
  create public .

*"* public components of class Z6HR006C_LEAVE_REQ_SCREEN
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_CONTEXT .
  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PT_GEN_REQ .

  data MESSAGE_HANDLER type ref to IF_PT_REQ_MESSAGE_HANDLER .

  methods CONSTRUCTOR .
protected section.
*"* protected components of class Z6HR006C_LEAVE_REQ_SCREEN
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR006C_LEAVE_REQ_SCREEN
*"* do not include other source files here!!!

  class-data ACTOR_AGENT type ref to CA_PT_REQ_ACTOR .
  data FUNCTIONAL_EXIT_GEN type ref to IF_EX_PT_GEN_REQ .
ENDCLASS.



CLASS Z6HR006C_LEAVE_REQ_SCREEN IMPLEMENTATION.


method CONSTRUCTOR.
*---Create message handler singleton
  CLASS cl_pt_req_message_handler DEFINITION LOAD.
  CALL METHOD cl_pt_req_message_handler=>instance_get
    RECEIVING
      result = message_handler.

  CLASS cl_pt_req_actor DEFINITION LOAD.
  actor_agent   = ca_pt_req_actor=>agent.

endmethod.


method IF_EX_PT_GEN_REQ~CHECK_IF_ACTOR_ABSENT.
* This BAdI determines if the given person is available
* within a period of 14 days starting today. If the person
* is not available (absent), RESULT will be set to 'X'

  DATA: l_begda TYPE bapi7002_3-startdate,
        l_endda TYPE bapi7002_3-enddate,
        l_persno TYPE TABLE OF bapi7002_2,
        l_timeavailschedule TYPE TABLE OF bapi7002_1,
        l_timeavailschedule_wa LIKE LINE OF l_timeavailschedule,
        l_return TYPE TABLE OF bapiret2.

* set dates
  l_begda = sy-datum.
  l_endda = sy-datum + 14.

  APPEND im_pernr TO l_persno.

* determine availability
  CALL FUNCTION 'BAPI_TIMEAVAILSCHEDULE_BUILD'
    EXPORTING
      startdate         = l_begda
      enddate           = l_endda
    TABLES
      persno            = l_persno
      timeavailschedule = l_timeavailschedule
      return            = l_return.

* assume not available
  result = 'X'.
* check if available
  LOOP AT l_timeavailschedule TRANSPORTING NO FIELDS WHERE availability EQ 1.
    result = ' '.
    EXIT.
  ENDLOOP.
endmethod.


  method IF_EX_PT_GEN_REQ~CHECK_SELECTED_NEXT_PROCESSOR.
  endmethod.


  method IF_EX_PT_GEN_REQ~FILTER_NEXT_PROCESSOR.
  endmethod.


  method IF_EX_PT_GEN_REQ~FILTER_REQUESTS_FOR_EMAILS.
  endmethod.


method IF_EX_PT_GEN_REQ~FIND_RESP_AND_DEFAULT_NEXT_PRC.
*DATA: l_ac_container    TYPE TABLE OF swcont INITIAL SIZE 0,
*        l_ac_container_wa TYPE swcont,
*        l_actor_tab       TYPE TABLE OF swhactor INITIAL SIZE 0,
*        l_actor_wa        TYPE swhactor,
*        l_resp_pernr      TYPE bapiusr01-employeeno,
*        l_actor           TYPE REF TO if_pt_req_a_wf,
*        lcl_msgv1         TYPE sy-msgv1,
*        lcl_msgv2         TYPE sy-msgv2,
*        l_approver_uname  TYPE sy-uname,
*        l_approver_pernr  TYPE bapiusr01-employeeno,
*        l_return          TYPE bapiret2.
*
**---Find actor for which to determine the responsible and default next processor
*  CALL METHOD actor_agent->create_actor
*    EXPORTING
*      im_actor_type      = im_actortype
*      im_otype           = im_otype
*      im_objid           = im_objid
*    IMPORTING
*      ex_actor           = l_actor
*    EXCEPTIONS
*      missing_parameter  = 1
*      pernr_not_existing = 2
*      application_error  = 3
*      OTHERS             = 4.
*  IF sy-subrc <> 0.
**---ERROR: actor could not be found
*    IF 1 = 2.
**---Workaround for where-used list
*      MESSAGE e036(hrtim_abs_req) WITH '&'.
*    ENDIF.
*    CONCATENATE im_actortype im_otype im_objid INTO lcl_msgv1.
*    CALL METHOD message_handler->add_message
*      EXPORTING
*        im_type       = 'E'
*        im_cl         = 'HRTIM_ABS_REQ'
*        im_number     = '036'
*        im_par1       = lcl_msgv1
*        im_classname  = 'CL_PT_ARQ_REQ_EXIT'
*        im_methodname = 'IF_EX_PT_ABS_REQ~FIND_RESP_AND_DEFAULT_NEXT_PRC'.
**---No sense to continue --> actor for which responsible and next processor
**   should be determined could not be found
*    RETURN.
*  ENDIF.
*
**TODO - BEGIN: Remove deactivated coding
***----USER must exist for actor - if it doesn't exist --> exit
**  IF l_actor->user IS INITIAL.
***---ERROR (user not available)
**    IF 1 = 2.
***---Workaround for where-used list
**      MESSAGE e009(hrtim_abs_req) WITH '&'.
**    ENDIF.
**    lcl_msgv1 = l_actor->pernr.
**    CALL METHOD message_handler->add_message
**      EXPORTING
**        im_type       = 'E'
**        im_cl         = 'HRTIM_ABS_REQ'
**        im_number     = '009'
**        im_par1       = lcl_msgv1
**        im_context    = 'EXIT'
**        im_subcontext = 'ACTOR'
**        im_classname  = 'CL_PT_ARQ_REQ_EXIT'
**        im_methodname = 'IF_EX_PT_ABS_REQ~FIND_RESP_AND_DEFAULT_NEXT_PRC'.
**    RETURN.
**  ENDIF.
**TODO - END: Remove deactivated coding
*
**---Determine manager (responsible actor)
*  l_ac_container_wa-element    = 'ORG_OBJECT'.
*  l_ac_container_wa-elemlength = '025'.
*  l_ac_container_wa-type       = 'C'.
*  IF NOT l_actor->user IS INITIAL.
**---User ID is provided --> use it
*    CONCATENATE 'US' l_actor->user INTO l_ac_container_wa-value.
*  ELSE.
**---No user ID provided --> use personnel number instead
*    CONCATENATE 'P' l_actor->pernr INTO l_ac_container_wa-value SEPARATED BY space.
*  ENDIF.
*  APPEND l_ac_container_wa TO l_ac_container.
*  CALL FUNCTION 'SWX_GET_MANAGER'
*    TABLES
*      actor_tab    = l_actor_tab
*      ac_container = l_ac_container
*    EXCEPTIONS
*      nobody_found = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
**TODO - BEGIN: Remove deactivated coding
***---ERROR: Manager could not be determined
**    IF 1 = 2.
***---Workaround for where-used list
**      MESSAGE e039(hrtim_abs_req) WITH '&' '&'.
**    ENDIF.
**    lcl_msgv1 = l_actor->user.
**    lcl_msgv2 = im_date.
**    CALL METHOD message_handler->add_message
**      EXPORTING
**        im_type       = 'E'
**        im_cl         = 'HRTIM_ABS_REQ'
**        im_number     = '039'
**        im_par1       = lcl_msgv1
**        im_par2       = lcl_msgv2
**        im_context    = 'EXIT'
**        im_subcontext = 'ACTOR'
**        im_classname  = 'CL_PT_ARQ_REQ_EXIT'
**        im_methodname = 'IF_EX_PT_ABS_REQ~FIND_RESP_AND_DEFAULT_NEXT_PRC'.
**    RETURN.
**TODO - END: Remove deactivated coding
*  ELSE.
**---Manager found --> determine responsible actor
*    LOOP AT l_actor_tab INTO l_actor_wa.
*      IF l_actor_wa-otype EQ 'P '.
*        l_resp_pernr = l_actor_wa-objid.
*        CALL METHOD actor_agent->create_actor
*          EXPORTING
*            im_actor_type      = 'P'
*            im_otype           = 'P'
*            im_objid           = l_resp_pernr
*          IMPORTING
*            ex_actor           = ex_responsible_actor
*          EXCEPTIONS
*            missing_parameter  = 1
*            pernr_not_existing = 2
*            application_error  = 3
*            OTHERS             = 4.
*        IF sy-subrc <> 0.
**---ERROR: responsible actor could not be created or determined
*          IF 1 = 2.
**---Workaround for where-used list
*            MESSAGE e036(hrtim_abs_req) WITH '&'.
*          ENDIF.
*          CONCATENATE 'P' 'P' l_approver_pernr INTO lcl_msgv1.
*          CALL METHOD message_handler->add_message
*            EXPORTING
*              im_type       = 'E'
*              im_cl         = 'HRTIM_ABS_REQ'
*              im_number     = '036'
*              im_par1       = lcl_msgv1
*              im_context    = 'EXIT'
*              im_subcontext = 'ACTOR'
*              im_classname  = 'CL_PT_ARQ_REQ_EXIT'
*              im_methodname = 'IF_EX_PT_ABS_REQ~FIND_RESP_AND_DEFAULT_NEXT_PRC'.
*        ENDIF.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
**---Get most recently selected default next processor
*  SELECT SINGLE approver FROM  hressapprover
*                         INTO  l_approver_uname
*                        WHERE  inituser  = l_actor->user.
*  IF ( sy-subrc EQ 0 AND NOT ex_responsible_actor IS INITIAL ) AND
*     ( l_approver_uname EQ ex_responsible_actor->user OR l_approver_uname IS INITIAL ).
**---Selected next processor is the responsible --> delete superfluous entry in HRESSAPPROVER
*    DELETE FROM hressapprover WHERE inituser = l_actor->user.
*    ex_def_next_proc_actor = ex_responsible_actor.
*    RETURN.
*  ENDIF.
*
**---No entry exists --> use responsible (if existing) as default next processor and exit
*  IF sy-subrc NE 0 AND NOT ex_responsible_actor IS INITIAL.
*    ex_def_next_proc_actor = ex_responsible_actor.
*    RETURN.
*  ENDIF.
*
**---No sense to determine personnel number and actor for default next processor if it doesn't exist
*  CHECK NOT l_approver_uname IS INITIAL.
*
**---Determine personnel number of default next processor
*  CALL FUNCTION 'BAPI_USR01DOHR_GETEMPLOYEE'
*    EXPORTING
*      id             = l_approver_uname
*      begindate      = im_date
*      enddate        = im_date
*    IMPORTING
*      return         = l_return
*      employeenumber = l_approver_pernr.
*  IF l_approver_pernr IS INITIAL.
**---ERROR: Personnel number for next processor could not be determined
*    IF 1 = 2.
**---Workaround for where-used list
*      MESSAGE e037(hrtim_abs_req) WITH '&' '&'.
*    ENDIF.
*    lcl_msgv1 = l_approver_uname.
*    lcl_msgv2 = im_date.
*    CALL METHOD message_handler->add_message
*      EXPORTING
*        im_type       = 'I'
*        im_cl         = 'HRTIM_ABS_REQ'
*        im_number     = '037'
*        im_par1       = lcl_msgv1
*        im_par2       = lcl_msgv2
*        im_context    = 'REQUEST'
*        im_classname  = 'CL_PT_ARQ_REQ'
*        im_methodname = 'IF_EX_PT_ABS_REQ~GET_DEFAULT_NEXT_PROCESSOR'.
**---No sense to continue with determination of next processor
*    RETURN.
*  ENDIF.
*
**---Determine default next processor actor for the personnel number
*  CALL METHOD actor_agent->create_actor
*    EXPORTING
*      im_actor_type      = 'P'
*      im_otype           = 'P'
*      im_objid           = l_approver_pernr
*    IMPORTING
*      ex_actor           = ex_def_next_proc_actor
*    EXCEPTIONS
*      missing_parameter  = 1
*      pernr_not_existing = 2
*      application_error  = 3
*      OTHERS             = 4.
*  IF sy-subrc <> 0.
**---ERROR: next processor actor could not be created or determined
*    IF 1 = 2.
**---Workaround for where-used list
*      MESSAGE e036(hrtim_abs_req) WITH '&'.
*    ENDIF.
*    CONCATENATE 'P' 'P' l_approver_pernr INTO lcl_msgv1.
*    CALL METHOD message_handler->add_message
*      EXPORTING
*        im_type       = 'E'
*        im_cl         = 'HRTIM_ABS_REQ'
*        im_number     = '036'
*        im_par1       = lcl_msgv1
*        im_context    = 'REQUEST'
*        im_classname  = 'CL_PT_ARQ_REQ'
*        im_methodname = 'IF_EX_PT_ABS_REQ~GET_DEFAULT_NEXT_PROCESSOR'.
**    RETURN.
*  ENDIF.

endmethod.


method IF_EX_PT_GEN_REQ~GET_ACTOR_SUBSTITUTES.
  DATA: l_subst_str TYPE struc,
        l_class     TYPE p1217-tclass,
        i77ro       TYPE t77ro,
        l_profile   TYPE t77ro-reppr.

  IF NOT im_task IS INITIAL.
* get classification of task
    CALL FUNCTION 'SWD_GET_TASK_ATTRIBUTES'
      EXPORTING
        im_task  = im_task
      IMPORTING
        ex_class = l_class
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      l_class = '000000000003'. "disciplinary
    ENDIF.

* get profile assigned to class
    SELECT SINGLE * FROM  t77ro INTO i77ro
           WHERE  tclass  = l_class.
    l_profile = i77ro-reppr.
    IF sy-subrc <> 0.
      l_profile = '0002'. "disciplinary
    ENDIF.
  ELSE.
    l_profile = 'ALL'. "all
  ENDIF.

  CALL FUNCTION 'RH_SUBSTITUTES_GET'
    EXPORTING
      act_plvar                 = '  '
      act_otype                 = 'US'
      act_objid                 = im_user
*   ACT_BEGDA                 = SY-DATUM
*   ACT_ENDDA                 = '99991231'
     show_holder_flag          = 'X'
     authority_check           = 'X'
   TABLES
     subst_obj                 = ex_subst_obj
     subst_str                 = ex_subst_str
   EXCEPTIONS
     no_substitute_found       = 1
     OTHERS                    = 2
            .
  IF sy-subrc = 0.
    LOOP AT ex_subst_str INTO l_subst_str.
      IF l_subst_str-vadata(4) <> l_profile AND    "<<<<< exchange Profile here!
        NOT l_subst_str-vadata(4) IS INITIAL.      "ANK Note 1044996
        DELETE ex_subst_obj INDEX sy-tabix.
        DELETE ex_subst_str INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.
endmethod.


method IF_EX_PT_GEN_REQ~GET_ADMINS.
* XML 20041005 NOTE 779409 Emails reports improvements

*                                                                       "BEGIN INS NOTE779409
  DATA:
    wa_receivers TYPE somlreci1.

  IF NOT im_next_processor-internet_address IS INITIAL.
    wa_receivers-receiver = im_next_processor-internet_address.
    wa_receivers-rec_type = 'U'.
    wa_receivers-com_type = 'INT'.
    APPEND wa_receivers TO ch_receivers.
  ENDIF.

  CASE im_itemtype.
    WHEN cl_pt_arq_const=>c_req_type.

    WHEN cl_pt_cor_const=>c_req_type.

  ENDCASE.
endmethod.


method IF_EX_PT_GEN_REQ~GET_EMPLOYEE_COMMUNICATION.
  data : lt_comm  TYPE TABLE OF P0105,
        lcl_comm LIKE LINE OF lt_comm.

  CALL FUNCTION 'PT_ARQ_READ_INFOTYPES'
    EXPORTING
      pernr           = im_personnelnumber
      BEGDA           = sy-datum
      ENDDA           = sy-datum
*   IMPORTING
*     RETURN          =
   TABLES
      ITAB_0105       = lt_comm.

  read table lt_comm with key usrty = '0010' into lcl_comm.

  ex_reqs-usrid_long =  lcl_comm-usrid_long.

endmethod.


method IF_EX_PT_GEN_REQ~MODIFY_APPLICATION_MESSAGES.
  DATA : WA_MESSAGE_TAB TYPE PTREQ_MESSAGE_STRUC.
  CHECK IM_APPID EQ 'ESS_LR'.

  READ TABLE CH_MESSAGE_TAB INTO WA_MESSAGE_TAB WITH KEY TYPE = 'E'
                          ID = 'HRTIM00REC'
                          NUMBER = '012'.

  IF SY-SUBRC EQ 0 and ( wa_message_tab-message_v1 eq  'SL1'
                           or wa_message_tab-message_v1 eq  'SL2'
                           or wa_message_tab-message_v1 eq  'CL1'
                           or wa_message_tab-message_v1 eq  'CL2').

    WA_MESSAGE_TAB-TYPE = 'W'.
    MODIFY CH_MESSAGE_TAB FROM WA_MESSAGE_TAB index sy-tabix TRANSPORTING TYPE.
    CLEAR WA_MESSAGE_TAB.
  ENDIF.
endmethod.


method IF_EX_PT_GEN_REQ~SEARCH_FOR_NEXT_PROCESSOR.
* Start of ANK 1061925
  DATA itab_t77s0 type table of t77s0.
  DATA wa_t77s0 like line of itab_t77s0.
  DATA wa_persdata like line of ex_persdata_tab.
  DATA itab_0000 type table of P0000.
  DATA wa_0000 like line of itab_0000.
  DATA ret like sy-subrc.
  SELECT SINGLE * FROM t77s0 INTO wa_t77s0 WHERE grpid EQ 'ESS'
                                           AND   semid EQ 'STAT2'.
  IF sy-subrc IS INITIAL.
    LOOP AT ex_persdata_tab INTO wa_persdata.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          PERNR     = wa_persdata-pernr
          INFTY     = '0000'
          BEGDA     = sy-datum
          ENDDA     = sy-datum
        IMPORTING
          SUBRC     = ret
        TABLES
          INFTY_TAB = itab_0000.
      IF ret IS INITIAL.
        READ TABLE itab_0000 INTO wa_0000 index 1.
        IF sy-subrc IS INITIAL.
          IF wa_0000-stat2 <> wa_t77s0-gsval.
            DELETE TABLE ex_persdata_tab FROM wa_persdata.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR itab_0000.                  "JAIN_1162286
    ENDLOOP.
  ENDIF.
* End of ANK 1061925
endmethod.


method IF_EX_PT_GEN_REQ~START_WF.
* 500 SP04
* XRK 20040926 Note776883 Add workflow error messages to message handler

  DATA: im_agent_container    TYPE REF TO if_swf_ifs_parameter_container,
        lc_workitem_container TYPE REF TO if_swf_cnt_container,
        agents                TYPE TABLE OF swhactor INITIAL SIZE 0,
        l_creator             TYPE swwwihead-wi_creator,
        workitem_id           TYPE sww_wiid,
        lc_workflow           TYPE REF TO cl_wfd_adhoc_workflow_start,
        lc_step               TYPE REF TO if_wfd_adhoc_step,
        l_tabix               TYPE sy-tabix,
        l_agent               TYPE swdaagnt,
        l_n_processor         TYPE ptreq_actor_struc_flat,
        lc_agent_container    TYPE REF TO if_swf_cnt_container,
        lcl_attribs           TYPE ptreq_request_struc_flat,
        lcl_wf_attribs        TYPE REF TO cl_pt_req_wf_attribs,
        lcl_lpor              TYPE sibflpor,
        lcl_bi_persistent     TYPE REF TO bi_persistent,
        lcl_workarea          TYPE REF TO cl_pt_req_header,
        lt_workflow_container TYPE swrtcont,
        lt_wf_container_obj   TYPE swrtcont,
        lt_wf_container_por   TYPE swrtcont,
        lt_errors             TYPE swft100tab.
  DATA: lt_wf_error_tab       TYPE TABLE OF swr_mstruc,         "INS Note776883
        lt_wf_error_line      TYPE swr_mstruc.                  "INS Note776883

  DATA: exit_gen TYPE REF TO pt_gen_req.
  DATA: re_absent TYPE boole_d.                                 "ANK Note962002
  DATA: user TYPE sy-uname.                                     "ANK Note962002
  DATA: ex_subst_str TYPE struc_t.                              "ANK Note962002
  DATA: subst_settings TYPE padd2.                              "ANK Note962002
  DATA: wa_ex_subst TYPE LINE OF struc_t.                       "ANK Note962002
  DATA: application TYPE REF TO cl_pt_req_application.

  DATA: ex_subst_obj TYPE objec_t.                              "ANK Note1044996
  DATA: wa_ex_subst_obj TYPE LINE OF objec_t.                   "ANK Note1044996
  DATA: l_index type sy-tabix.                                  "ANK Note1044996


* RETURN_CODE logic:
*  = 1 agent missing error
*  = 2 data container error
*  = 3 WF start failed
*  = 4 some other error

  application = cl_pt_req_application=>get_instance( ).

* call exit
  exit_gen = application->functional_exit_gen.

*create workflow-start object
  CREATE OBJECT lc_workflow
    EXPORTING
      task                          = im_main_task
    EXCEPTIONS
      task_without_adhoc_capability = 1
      OTHERS                        = 2.
  IF sy-subrc <> 0.
    return_code = 4.
    EXIT.
  ENDIF.


* get all step objects and display the assigned agents
  DO.
    ADD 1 TO l_tabix.
* get ad-hoc-workflow-step
    CALL METHOD lc_workflow->step_by_index_get
      EXPORTING
        index         = l_tabix
      RECEIVING
        step          = lc_step
      EXCEPTIONS
        invalid_index = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

* set user for ad-hoc-agent --> NEXT PROCESSOR
    CALL METHOD cl_pt_gen_badi=>get_next_processor
      EXPORTING
        im_request       = im_request
      IMPORTING
        ex_actor_attribs = l_n_processor.

    CALL BADI exit_gen->check_if_actor_absent  "start of ANK962002
      EXPORTING
        im_pernr         = l_n_processor-pernr
      RECEIVING
        result           = re_absent.
* If the next processor is absent for 14 days, then determine substitute for him/her.
    IF re_absent = 'X'.
      user = l_n_processor-user.
      CALL BADI exit_gen->get_actor_substitutes
        EXPORTING
          im_user      = user
        IMPORTING
          ex_subst_str = ex_subst_str
          ex_subst_obj = ex_subst_obj.                                  "ANK Note1044996

*     CHECK NOT ex_subst_str IS INITIAL.                                "AVI_1113279
      IF    NOT ex_subst_str IS INITIAL.                                "AVI_1113279
      LOOP AT ex_subst_str INTO wa_ex_subst.
        subst_settings = wa_ex_subst-vadata.
        CHECK NOT subst_settings-active IS INITIAL. "Consider only active substitute

        IF wa_ex_subst-otype = 'US'.
          CLEAR l_agent-agent.
          READ TABLE ex_subst_obj INTO wa_ex_subst_obj INDEX l_index.                        "ANK Note1044996
          CONCATENATE wa_ex_subst-otype wa_ex_subst_obj-realo INTO l_agent-agent.            "ANK Note1044996

          CONCATENATE wa_ex_subst-otype wa_ex_subst-objid INTO l_agent-agent.
          CALL METHOD lc_step->agent_append
            EXPORTING
              agent          = l_agent
            EXCEPTIONS
              already_exists = 1
              locked         = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            return_code = 1.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      ELSE.                                                              "Begin AVI_1113279
        CONCATENATE 'US' l_n_processor-user INTO l_agent-agent.
* append agent to processor-list
        CALL METHOD lc_step->agent_append
          EXPORTING
            agent          = l_agent
          EXCEPTIONS
            already_exists = 1
            locked         = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          return_code = 1.
          EXIT.
        ENDIF.
      ENDIF.                                                             "End AVI_1113279
    ELSE.
      CONCATENATE 'US' l_n_processor-user INTO l_agent-agent.
* append agent to processor-list
      CALL METHOD lc_step->agent_append
        EXPORTING
          agent          = l_agent
        EXCEPTIONS
          already_exists = 1
          locked         = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        return_code = 1.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.                        "end of ANK962002


* get agents (container format)
  CALL METHOD lc_workflow->agents_as_container_get
    IMPORTING
      container      = im_agent_container
    EXCEPTIONS
      agents_missing = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    return_code = 1.
    EXIT.
  ENDIF.

  TRY.
      CALL METHOD cl_swf_cnt_factory=>create_task_container
        EXPORTING
          im_task_id        = im_main_task
        IMPORTING
          ex_task_container = lc_workitem_container.
    CATCH  cx_swf_utl_obj_create_failed
           cx_swf_utl_no_plan_variant
           cx_swf_utl_task_not_found
           cx_swf_utl_obj_invalid_ref .
      return_code = 2.
      EXIT.
  ENDTRY.

* merge agents to container
  IF im_agent_container IS BOUND.
    lc_agent_container ?= im_agent_container.
    TRY.
        CALL METHOD lc_workitem_container->merge
          EXPORTING
            other_container = lc_agent_container.
      CATCH cx_swf_cnt_container.
        return_code = 2.
        EXIT.
    ENDTRY.
  ENDIF.

*---Set workflow attributes
  lcl_lpor-instid = im_request->if_pt_req_request~request_id.
  CALL METHOD cl_pt_req_wf_attribs=>bi_persistent~find_by_lpor
    EXPORTING
      lpor   = lcl_lpor
    RECEIVING
      result = lcl_bi_persistent.
  lcl_wf_attribs ?= lcl_bi_persistent.
  TRY.
      CALL METHOD lc_workitem_container->element_set
        EXPORTING
          name  = 'Req'
          value = lcl_wf_attribs.
    CATCH  cx_swf_cnt_cont_access_denied
           cx_swf_cnt_elem_not_found
           cx_swf_cnt_elem_access_denied
           cx_swf_cnt_elem_type_conflict
           cx_swf_cnt_unit_type_conflict
           cx_swf_cnt_elem_def_invalid
           cx_swf_cnt_container .
      return_code = 2.
      EXIT.
  ENDTRY.

*---Fill simple workflow container
  CALL METHOD lc_workitem_container->to_simple_container
    EXPORTING
      import_param            = 'X'
      export_param            = 'X'
      changing_param          = 'X'
      returning_param         = 'X'
      no_system_elements      = space
      no_initial_elements     = 'X'
    IMPORTING
      ex_container_values     = lt_workflow_container
      ex_container_sibflporbs = lt_wf_container_por
      ex_t_messages           = lt_errors.
  APPEND LINES OF lt_wf_container_por TO lt_workflow_container.

*---Start workflow
  CALL FUNCTION 'SAP_WAPI_START_WORKFLOW'
    EXPORTING
      task               = im_main_task
      do_commit          = space
      start_asynchronous = 'X'
    IMPORTING
      return_code        = return_code
      workitem_id        = workitem_id
    TABLES
      input_container    = lt_workflow_container
      message_struct     = lt_wf_error_tab.                     "INS Note776883

  IF return_code NE 0.
    LOOP AT lt_wf_error_tab INTO lt_wf_error_line.              "INS Note776883
*---Add message to message handler                              "INS Note776883
      CALL METHOD message_handler->add_message                  "INS Note776883
        EXPORTING                                               "INS Note776883
          im_type       = lt_wf_error_line-msgty                "INS Note776883
          im_cl         = lt_wf_error_line-msgid                "INS Note776883
          im_number     = lt_wf_error_line-msgno                "INS Note776883
          im_par1       = lt_wf_error_line-msgv1                "INS Note776883
          im_par2       = lt_wf_error_line-msgv2                "INS Note776883
          im_par3       = lt_wf_error_line-msgv3                "INS Note776883
          im_par4       = lt_wf_error_line-msgv4                "INS Note776883
          im_classname  = 'CL_DEF_IM_PT_GEN_REQ'                "INS Note776883
          im_methodname = 'IF_EX_PT_GEN_REQ~START_WF'.          "INS Note776883
    ENDLOOP.                                                    "INS Note776883
    return_code = 3.
  ENDIF.

  CALL METHOD im_request->set_workitem_id
    EXPORTING
      im_workitem_id = workitem_id.

endmethod.
ENDCLASS.
