
-- intermediate input:
-- "1234567890 For 12345678901 example, in this sentence, there are 9 words. Though we have yet to try the maximization of this example!"


-- example input:
-- [
--   "    1234567890        For 12345678901        ",
--   "     example, in this     ",
--   "sentence, there are 9 words.",
--   "Though we have yet to try the maximization",
--   "of this example!"
--  ]

-- example output:
-- [
--   "1234567890",
--   "For"
--   "12345678901"
--   "example, in"
--   "this"
--   "sentence,"
--   "there are 9"
--   "words."
--   "Though we"
--   "have yet to"
--   "try the"
--   "maximizati-"
--   "on of this"
--   "example!"
-- ]


-- Input: ["The more I learn, the more I know.","The more I know, the more I forget."]

-- intermediate input: ["The","more","I","learn,","the","more","I","know.","The","more","I","know,","the","more","I","forget."]

-- Output: (["The","more","I","learn,","the","know.","know,","forget."],[1,2,3,4,5,2,3,6,1,2,3,7,5,2,3,8])


-- input: decode (["The","more","I","learn,","the","know.","know,","forget."], [1,2,3,4,5,2,3,6,1,2,3,7,5,2,3,8])

-- output: "The more I learn, the more I know. The more I know, the more I forget."
