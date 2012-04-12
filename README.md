HTML2XML
--------

> Convert your invalid XML to valid XML with the power of TagSoup

Example:

    repair :: StringLike s => s -> s
    repair "<head><p></head>" -- Outputs "<head><p></p></head>"
