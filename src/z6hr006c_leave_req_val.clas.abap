class Z6HR006C_LEAVE_REQ_VAL definition
  public
  final
  create public .

*"* public components of class Z6HR006C_LEAVE_REQ_VAL
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PT_ABS_REQ .
protected section.
*"* protected components of class Z6HR006C_LEAVE_REQ_VAL
*"* do not include other source files here!!!
private section.
*"* private components of class Z6HR006C_LEAVE_REQ_VAL
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z6HR006C_LEAVE_REQ_VAL IMPLEMENTATION.


  method IF_EX_PT_ABS_REQ~ADD_ACCRUAL_TO_QUOTA_BAL.
  endmethod.


  method IF_EX_PT_ABS_REQ~AUTH_CHECK_AND_ANONYMIZE_DATA.
  endmethod.


  method IF_EX_PT_ABS_REQ~CHECK_ATTACHMENT_MANDATORY.
  endmethod.


  method IF_EX_PT_ABS_REQ~CHECK_DUPLICATED_APPROVERS.
  endmethod.


  method IF_EX_PT_ABS_REQ~CHECK_TIME_CONSTRAINTS.
  endmethod.


  method IF_EX_PT_ABS_REQ~CHECK_TIME_CONSTR_FOR_SUBTY.
  endmethod.


  method IF_EX_PT_ABS_REQ~CONVERT_FILE_FORMAT.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_ATTATCHMENT_CONFIG.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_FIELD_SELECTION_ATTRIBS.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_IT0005_QUOTAS.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_MOD_AND_QTYPE.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_MULTIPLE_APPROVERS.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_TIMETYPES_AS_QUOTAS.
  endmethod.


  method IF_EX_PT_ABS_REQ~GET_WORKFLOW_ATTRIBS.
  endmethod.


  method IF_EX_PT_ABS_REQ~INDIRECT_SUBST_APPR.
  endmethod.


  method IF_EX_PT_ABS_REQ~POST_VIA_BLOP.
  endmethod.


  method IF_EX_PT_ABS_REQ~PROCESS_MESSAGES.
  endmethod.


  method IF_EX_PT_ABS_REQ~SET_CALENDAR_COLOR.
  endmethod.


  method IF_EX_PT_ABS_REQ~SET_CALENDAR_LEGEND_COLORS.
  endmethod.


  method IF_EX_PT_ABS_REQ~SET_CALENDAR_OVERLAPPING_COLOR.
  endmethod.


  method IF_EX_PT_ABS_REQ~SET_OVERVIEW_LEGEND_COLORS.
  endmethod.


method IF_EX_PT_ABS_REQ~SIMULATE_VIA_BLOP.
endmethod.


  method IF_EX_PT_ABS_REQ~TEAM_CALE_ENRICHMENT.
  endmethod.


  method IF_EX_PT_ABS_REQ~UPDATE_MULTIPLE_APPROVERS.
  endmethod.


  method IF_EX_PT_ABS_REQ~VALIDATE_AND_SIMULATE.
  endmethod.
ENDCLASS.
