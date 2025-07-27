
# Documentation Writing Guidelines

When writing conceptual documentation for this project, follow these principles to create clear, useful explanations:

## Intent-First Communication
- Lead with the "why" before the "how" - establish what we're trying to accomplish before explaining methods
- Help readers understand the reasoning behind design decisions and transformations
- Structure explanations as: purpose/intent → specific techniques used to achieve that purpose

## Concrete Specificity Over Abstract Generality
- Replace vague programming terminology with domain-specific explanations of actual transformations and analyses
- Use concrete examples when possible (e.g., "`array[i][j]` becomes `array[i*width + j]`")
- Avoid generic terms like "functionality that flows" or "modules provide capabilities"
- Use descriptive section titles that summarize content essence rather than generic labels

## Established Domain Language
- Use the project's established terminology consistently (e.g., "Memory Access Protocol" not "CUDA kernel represented as a protocol")
- Leverage known terms that don't need re-explanation for clearer communication
- Prefer domain-specific language over programming terms (e.g., "memory access patterns," "precision tracking" over "module interactions," "data flows")

## Conceptual Focus Over Implementation Details
- Explain **what the system does and how it works** rather than how to use APIs or navigate code
- Describe the intellectual approach and problem-solving strategy rather than code organization
- Focus on understanding the analytical process: "how does the system think about the problem"
- Help readers mentally model how the system works and why it makes specific decisions

## Clarity Through Structure
- Organize information to build understanding progressively
- Every section should teach concepts that help readers reason about what the system does and why it works
- Avoid organizational sections that just categorize information without explaining substance
- Rewrite confusing sentences clearly, even if it requires more words
- Eliminate unclear metaphors and vague descriptions

## Eliminate Redundant Qualifiers
- Remove unnecessary phrases that don't add meaning (e.g., "throughout the pipeline" when pipeline IS the analysis)
- Question every qualifier - does it actually clarify or just add verbosity?
- Choose the most direct expression: "analysis decisions" → "analysis"
- Eliminate redundant phrasing between sentences - vary terminology when expressing similar concepts
- Prefer simpler terminology over complex adjectives (e.g., "actual" vs "concrete")

## Use Domain-Precise Terminology
- Replace generic technical terms with domain-specific ones that convey exact meaning
- "Precision tracking" → "overapproximation tracking" (explains what kind of precision we care about)
- "Mathematical results" → "exact costs" (specifies what mathematical concept matters)
- Connect to established standards and well-known concepts from the problem domain (e.g., C's `sizeof` operator)

## Question Every Abstract Term
- Challenge vague terms like "configuration parameters" - be specific about what parameters and why they matter
- Replace generic programming concepts with concrete domain actions
- Ask: "What specifically does this accomplish?" rather than "How is this implemented?"
- Focus on analytical reasoning rather than system mechanics - implementation details like error handling are often too low-level for architectural documentation

## Avoid Vague Pronouns and Sentence Starters
- Avoid using "it" when a clear subject noun works better
- Example: "converts it into a form" → "converts the protocol into a form"
- **DO NOT START SENTENCES WITH IT/THIS/THAT** - use specific subject nouns instead
- Example: "This improves clarity" → "Clear subject nouns improve clarity"
- Example: "It enables exact bounds" → "Our analysis enables exact bounds"
- Always refer to the referent explicitly for maximum clarity

## Minimize Colon-Separated Sentences
- **AVOID: Do not use colons to separate sentences in new writing - prefer periods and new sentences**
- Discourage writing sentences that use colons to separate independent clauses
- While existing colon-separated sentences are acceptable, prefer alternative structures for new writing
- Use period separation, semicolons, or restructure as multiple sentences for better readability
- Example: "We establish soundness: static analysis implies dynamic execution" → "We establish soundness. Static analysis implies dynamic execution."

## Structure with Clear Roadmaps
- Lead sections with roadmaps that list what will be covered
- Use format: "To achieve [goal], several [actions] are needed: [A], [B], [C]"
- Example: "To prepare Memory Access Protocols for Resource Calculus Generation, several transformations are needed: array filtering, constant folding and propagation, memory access flattening, and loop uniformization"
- This gives readers immediate orientation before diving into details
- Create proper section roadmaps that explain WHY each subsection is needed, not just WHAT it does
- Maintain clear WHY/WHAT/HOW structure in section introductions

## Connect Stages Through Requirements
- Explain each stage in terms of what the next stage requires
- Show how current stage constraints are driven by subsequent stage needs
- Avoid subjective qualifiers like "most sophisticated" - use objective descriptions
- Focus on necessity and causation rather than implementation complexity

## Use Parenthetical Clarification
- Clarify technical terms with parenthetical explanations
- Example: "hardware-level memory accesses (multi-dimensional arrays converted to flat memory addresses)"
- This helps readers understand domain-specific terminology without lengthy explanations

# Advanced Documentation Patterns

## Intent-First Problem-Solution Structure
- When explaining complex transformations, follow the pattern: state objective → identify problem → present solution
- Don't mechanically describe what the code does - explain WHY the transformation is necessary
- Example: "To translate loops into summations [objective], the encoding must handle the mismatch between loop ranges (which can have arbitrary step values) and mathematical summations (which iterate over every element) [problem]. The solution applies the inverse of the step function [solution]."

## Define Before Use
- Always define technical terms before using them in explanations
- Introduce concepts when they first become relevant, not when they're first used
- Example: Define "step value (the amount added or multiplied per iteration)" before referring to "arbitrary step values"

## Precise Terminology Consistency
- Use "translate" for converting between representations in translation contexts
- Use "encode" for encoding processes  
- Avoid generic terms like "convert" that may have other technical meanings
- Replace vague subjects like "the system" with specific actors: "the translation", "the encoding", or "we"
- Be consistent with technical vocabulary throughout the document

## Mathematical Formalization with Examples
- When presenting mathematical concepts, provide both formal definitions and concrete examples
- Make abstract concepts tangible through specific instances
- Example: Provide both the formal bank conflict formalization and concrete examples with specific index values

## Eliminate Unnecessary Qualifiers
- Avoid adjectives like "complex", "sophisticated", "mathematical" unless they add specific technical meaning
- Be concrete rather than descriptive - focus on what something does, not subjective characterizations
- Question every adjective: does it clarify the technical concept or just add noise?

## Scope Limitations Up Front
- State what's outside the document's scope early to set proper expectations
- Example: "Discussion of CoFloCo and KoAT is outside the scope of this document"

## Case-by-Case Structure for Algorithms
- For algorithms with multiple cases, use numbered cases with concrete examples
- Structure as: "Case 1: [condition] such as [example]. [What happens and why]."
- This is clearer than abstract algorithmic descriptions

## Connect to Established Techniques
- When using standard algorithms or transformations from established fields, explicitly mention the connection
- Reference the standard name/terminology from the relevant domain (compilers, databases, algorithms, etc.)
- This helps readers understand that the technique is well-established rather than novel
- Example: "We apply loop index normalization, a standard compiler technique, to convert loops with arbitrary step sizes into unit-stride loops"
- This grounds the work in existing knowledge and shows appropriate use of established methods

## Document Measured Results Over Predictions
- When performance or capability claims are made, include actual measurements when available
- Update documentation status as work progresses from planned to implemented (e.g., "(Planned)" → "(Implemented)")
- Let quantitative results speak for themselves rather than adding subjective qualifiers
- Example: "V1: 1.578 seconds, V2: 0.014 seconds, 113x speedup" rather than "massive performance improvement"
- Include benchmark methodologies and specific test conditions for reproducibility

The overarching goal is **conceptual clarity**: writing that helps someone understand the system's analytical reasoning and approach to solving problems, enabling them to mentally model how the system works rather than just describing its structure or features.