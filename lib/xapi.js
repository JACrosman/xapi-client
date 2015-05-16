/**
 The MIT License (MIT)

 Copyright (c) 2015 TryxAPI

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
 */
(function (xapi) {
    'use strict';

    function XApiError(message, code) {
        this.name = 'xAPIError';
        this.message = message;
        this.code = code;
    }

    XApiError.prototype = Error.prototype;

    xapi.Error = XApiError;
    xapi.errorCode = {
        // xAPI Errors business range from 0-999
        XAPI_PROPERTY_VIOLATION: 105,
        STATEMENTS_DUPLICATE_ID: 131,
        STATEMENT_INVALID_ID_TYPE: 96,
        STATEMENT_INVALID_ACTOR_TYPE: 97,
        STATEMENT_INVALID_VERB_TYPE: 98,
        STATEMENT_INVALID_OBJECT_TYPE: 99,
        STATEMENT_INVALID_RESULT_TYPE: 100,
        STATEMENT_INVALID_CONTEXT_TYPE: 101,
        STATEMENT_INVALID_AUTHORITY_TYPE: 102,
        STATEMENT_INVALID_TIMESTAMP_TYPE: 103,
        STATEMENT_INVALID_STORED_TYPE: 133,
        STATEMENT_INVALID_ATTACHMENT_ARRAY_TYPE: 104,
        STATEMENT_INVALID_ATTACHMENT_IN_ARRAY: 106,
        STATEMENT_ID_AND_VOIDED_STATEMENT_ID: 117,
        STATEMENT_INVALID_FORMAT_TYPE: 109,
        STATEMENT_INVALID_SUB_STATEMENT: 122,
        STATEMENT_INVALID_STATEMENT_REF: 123,
        STATEMENT_INVALID_AGENT_OR_GROUP: 124,
        STATEMENT_INVALID_ACTIVITY: 125,
        MISSING_ACTOR: 0,
        MISSING_VERB: 1,
        MISSING_OBJECT: 2,
        AGENT_INVALID_OBJECT_TYPE: 3,
        AGENT_MORE_THAT_ONE_IDENTIFIER: 4,
        AGENT_INVALID_MEMBER_TYPE: 58,
        AGENT_REQUIRED: 111,
        MISSING_IDENTIFIER: 5,
        AGENT_IDENTIFIER_INVALID_ACCOUNT_TYPE: 6,
        AGENT_IDENTIFIER_INVALID_OPENID: 7,
        AGENT_IDENTIFIER_INVALID_MBOX: 8,
        AGENT_IDENTIFIER_INVALID_MBOX_SHA1SUM: 59,
        AGENT_UNDETERMINED_OBJECT_TYPE: 9,
        AGENT_ANONYMOUS_GROUP_MEMBERS_AS_AGENTS: 10,
        AGENT_ANONYMOUS_GROUP_MUST_NOT_INCLUDE_FUNCTIONAL_IDENTIFIERS: 11,
        AGENT_INVALID_NAME_TYPE: 134,
        VERB_ID_NOT_IRI: 12,
        VERB_INVALID_DISPLAY_TYPE: 59,
        ACTIVITY_INVALID_OBJECT_TYPE: 13,
        ACTIVITY_INVALID_TYPE_ID: 14,
        ACTIVITY_MISSING_ID: 60,
        ACTIVITY_INVALID_DEFINITION: 61,
        OBJECT_INVALID_INTERACTION_COMPONENT_WHITESPACE_IN_ID: 15,
        OBJECT_INVALID_INTERACTION_COMPONENT_DUPLICATE_ID: 16,
        OBJECT_INVALID_SUB_STATEMENT: 17,
        OBJECT_INVALID_OBJECT_TYPE: 126,
        RESULTS_INVALID_DURATION: 18,
        RESULTS_INVALID_EXTENSIONS: 105,
        RESULTS_SCORE_INVALID_SCALED: 19,
        RESULTS_SCORE_INVALID_RAW: 89,
        RESULTS_SCORE_INVALID_MIN: 90,
        RESULTS_SCORE_INVALID_MAX: 91,
        RESULTS_SCORE_INVALID_MIN_MAX: 106,
        RESULTS_INVALID_SCORE: 92,
        RESULTS_INVALID_SUCCESS: 93,
        RESULTS_INVALID_COMPLETION: 94,
        RESULTS_INVALID_RESPONSE: 95,
        CONTEXT_INVALID_REVISION: 20,
        CONTEXT_INVALID_PLATFORM: 21,
        CONTEXT_INVALID_CONTEXT_ACTIVITY: 22,
        CONTEXT_INVALID_INSTRUCTOR_TYPE: 74,
        CONTEXT_INVALID_REGISTRATION_TYPE: 75,
        CONTEXT_INVALID_TEAM_TYPE: 76,
        CONTEXT_INVALID_CONTEXT_ACTIVITIES_TYPE: 77,
        CONTEXT_INVALID_REVISION_TYPE: 78,
        CONTEXT_INVALID_PLATFORM_TYPE: 79,
        CONTEXT_INVALID_LANGUAGE_TYPE: 80,
        CONTEXT_INVALID_STATEMENT_TYPE: 81,
        CONTEXT_INVALID_EXTENSION_TYPE: 82,
        TIMESTAMP_INVALID_ISO8601_FORMAT: 23,
        AUTHORITY_REQUIRED: 24,
        VERSION_INVALID: 25,
        OBJECT_INVALID_DEFINITION_INTERACTION_TYPE: 26,
        MISSING_STATEMENT: 27,
        MISSING_HOME_PAGE: 28,
        MISSING_NAME: 29,
        OBJECT_INVALID_STATEMENT_REF: 30,
        CONTEXT_LANGUAGE: 31,
        VERSION: 32,
        ACCOUNT_HOME_PAGE: 33,
        OBJECT_DEFINITION_INVALID_IRI: 34,
        OBJECT_INVALID_COMPONENT_ID: 35,
        OBJECT_INVALID_COMPONENT_ARRAY: 36,
        MUST_BE_BOOLEAN: 37,
        RAW_MUST_BE_WITHIN: 38,
        MIN_MUST_BE_LESS: 39,
        MAX_MUST_BE_GREATER: 40,
        EXTENSION_ID_NOT_IRI: 41,
        ATTACHMENT_USAGE_TYPE_NOT_IRI: 42,
        ATTACHMENT_FILE_URL_NOT_IRI: 43,
        ATTACHMENT_LENGTH_NOT_IRI: 44,
        ATTACHMENT_SHA2_REQUIRED: 45,
        ATTACHMENT_LENGTH_REQUIRED: 46,
        ATTACHMENT_DISPLAY_REQUIRED: 47,
        ATTACHMENT_USAGE_TYPE_REQUIRED: 48,
        ATTACHMENT_INVALID_DISPLAY_TYPE: 83,
        ATTACHMENT_INVALID_CONTENT_TYPE_TYPE: 84,
        ATTACHMENT_CONTENT_TYPE_REQUIRED: 85,
        ATTACHMENT_INVALID_LENGTH_TYPE: 86,
        ATTACHMENT_INVALID_SHA2_TYPE: 87,
        ATTACHMENT_INVALID_DESCRIPTION_TYPE: 88,
        EXTENSION_ID_NOT_OBJECT: 49,
        ATTACHMENT_NOT_ARRAY: 50,
        SUB_STATEMENT_NESTED: 51,
        GROUP_INVALID_OBJECT_TYPE: 52,
        STATEMENT_REF_REQUIRED: 53,
        LANGUAGE_MAP_NOT_OBJECT: 54,
        LANGUAGE_MAP_INVALID: 55,
        CONTEXT_REGISTRATION_UUID: 56,
        CONTEXT_CONTEXT_ACTIVITY_NOT_ACTIVITY: 57,
        DEFINITION_INVALID_NAME_TYPE: 62,
        DEFINITION_INVALID_DESCRIPTION_TYPE: 63,
        DEFINITION_INVALID_TYPE_TYPE: 64,
        DEFINITION_INVALID_MORE_INFO_TYPE: 65,
        DEFINITION_INVALID_EXTENSIONS_TYPE: 66,
        DEFINITION_INVALID_CORRECT_RESPONSE_PATTERN_TYPE: 67,
        DEFINITION_INVALID_CHOICES_TYPE: 68,
        DEFINITION_INVALID_SCALE_TYPE: 69,
        DEFINITION_INVALID_SOURCE_TYPE: 70,
        DEFINITION_INVALID_TARGET_TYPE: 71,
        DEFINITION_INVALID_STEP_TYPE: 72,
        DEFINITION_INVALID_INTERACTION_TYPE: 73,
        ACTIVITY_ID_INVALID_TYPE: 107,
        ACTIVITY_ID_REQUIRED: 108,
        ACTIVITY_ID_NOT_IRI: 117,
        LIMIT_INVALID_NUMBER: 110,
        STATE_ID_INVALID: 112,
        STATE_ID_REQUIRED: 113,
        PROFILE_ID_INVALID_TYPE: 114,
        PROFILE_ID_REQUIRED: 115,
        STATEMENT_QUERY_INVALID_PARAMS: 116,
        STATEMENT_INVALID_STATEMENT_ID_AND_VOIDED_STATEMENT_ID: 131,
        STATEMENT_INVALID_STATEMENT_ID_AND_EXTRA_ARGUMENTS: 132,
        STATEMENT_REF_INVALID_OBJECT_TYPE: 118,
        STATEMENT_REF_MISSING_OBJECT_TYPE: 119,
        STATEMENT_REF_MISSING_ID: 120,
        STATEMENT_REF_INVALID_ID: 121,
        AUTHORITY_AGENT: 126,
        AUTHORITY_GROUP: 127,
        AUTHORITY_OAUTH: 128,
        AUTHORITY_OAUTH_MEMBER_ACCOUNT: 129,
        AUTHORITY_OAUTH_ACCOUNT: 130,
        INTERACTION_COMPONENT_TYPE_ID: 135,
        INTERACTION_COMPONENT_INVALID_DESCRIPTION_TYPE: 136,
        DOCUMENT_ID_INVALID: 137,
        DOCUMENT_ID_REQUIRED: 138,
        DOCUMENT_UPDATED_INVALID: 139,
        DOCUMENT_UPDATED_REQUIRED: 140,
        DOCUMENT_CONTENTS_INVALID: 141,
        DOCUMENT_CONTENTS_REQUIRED: 142,
        DOCUMENT_INVALID_OBJECT_TYPE: 143,
        STATE_REQUIRED: 144,
        ACTIVITY_PROFILE_REQUIRED: 145,
        AGENT_PROFILE_REQUIRED: 146,
        NON_STRING_TYPE: 147,
        STATE_REGISTRATION_UUID: 148,
        SINCE_INVALID_ISO8601_FORMAT: 149,
        SINCE_INVALID: 150
    };
}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {})
));
/*
* A JavaScript implementation of the SHA256 hash function.
*
* FILE:	sha256.js
* VERSION:	0.8
* AUTHOR:	Christoph Bichlmeier <informatik@zombiearena.de>
* WEBSITE:  http://www.bichlmeier.info/sha256.js
*
* NOTE: This version is not tested thoroughly!
*
* Copyright (c) 2003, Christoph Bichlmeier
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
* 3. Neither the name of the copyright holder nor the names of contributors
*    may be used to endorse or promote products derived from this software
*    without specific prior written permission.
*
* ======================================================================
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
* OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
* BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
* OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/* SHA256 logical functions */
function rotateRight(n,x) {
	return ((x >>> n) | (x << (32 - n)));
}
function choice(x,y,z) {
	return ((x & y) ^ (~x & z));
}
function majority(x,y,z) {
	return ((x & y) ^ (x & z) ^ (y & z));
}
function sha256_Sigma0(x) {
	return (rotateRight(2, x) ^ rotateRight(13, x) ^ rotateRight(22, x));
}
function sha256_Sigma1(x) {
	return (rotateRight(6, x) ^ rotateRight(11, x) ^ rotateRight(25, x));
}
function sha256_sigma0(x) {
	return (rotateRight(7, x) ^ rotateRight(18, x) ^ (x >>> 3));
}
function sha256_sigma1(x) {
	return (rotateRight(17, x) ^ rotateRight(19, x) ^ (x >>> 10));
}
function sha256_expand(W, j) {
	return (W[j&0x0f] += sha256_sigma1(W[(j+14)&0x0f]) + W[(j+9)&0x0f] +
sha256_sigma0(W[(j+1)&0x0f]));
}

/* Hash constant words K: */
var K256 = new Array(
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
	0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
	0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
	0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
	0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
	0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
);

/* global arrays */
var ihash, count, buffer;
var sha256_hex_digits = "0123456789abcdef";

/* Add 32-bit integers with 16-bit operations (bug in some JS-interpreters:
overflow) */
function safe_add(x, y)
{
	var lsw = (x & 0xffff) + (y & 0xffff);
	var msw = (x >> 16) + (y >> 16) + (lsw >> 16);
	return (msw << 16) | (lsw & 0xffff);
}

/* Initialise the SHA256 computation */
function sha256_init() {
	ihash = new Array(8);
	count = new Array(2);
	buffer = new Array(64);
	count[0] = count[1] = 0;
	ihash[0] = 0x6a09e667;
	ihash[1] = 0xbb67ae85;
	ihash[2] = 0x3c6ef372;
	ihash[3] = 0xa54ff53a;
	ihash[4] = 0x510e527f;
	ihash[5] = 0x9b05688c;
	ihash[6] = 0x1f83d9ab;
	ihash[7] = 0x5be0cd19;
}

/* Transform a 512-bit message block */
function sha256_transform() {
	var a, b, c, d, e, f, g, h, T1, T2;
	var W = new Array(16);

	/* Initialize registers with the previous intermediate value */
	a = ihash[0];
	b = ihash[1];
	c = ihash[2];
	d = ihash[3];
	e = ihash[4];
	f = ihash[5];
	g = ihash[6];
	h = ihash[7];

        /* make 32-bit words */
	for(var i=0; i<16; i++)
		W[i] = ((buffer[(i<<2)+3]) | (buffer[(i<<2)+2] << 8) | (buffer[(i<<2)+1]
<< 16) | (buffer[i<<2] << 24));

        for(var j=0; j<64; j++) {
		T1 = h + sha256_Sigma1(e) + choice(e, f, g) + K256[j];
		if(j < 16) T1 += W[j];
		else T1 += sha256_expand(W, j);
		T2 = sha256_Sigma0(a) + majority(a, b, c);
		h = g;
		g = f;
		f = e;
		e = safe_add(d, T1);
		d = c;
		c = b;
		b = a;
		a = safe_add(T1, T2);
        }

	/* Compute the current intermediate hash value */
	ihash[0] += a;
	ihash[1] += b;
	ihash[2] += c;
	ihash[3] += d;
	ihash[4] += e;
	ihash[5] += f;
	ihash[6] += g;
	ihash[7] += h;
}

/* Read the next chunk of data and update the SHA256 computation */
function sha256_update(data, inputLen) {
	var i, index, curpos = 0;
	/* Compute number of bytes mod 64 */
	index = ((count[0] >> 3) & 0x3f);
        var remainder = (inputLen & 0x3f);

	/* Update number of bits */
	if ((count[0] += (inputLen << 3)) < (inputLen << 3)) count[1]++;
	count[1] += (inputLen >> 29);

	/* Transform as many times as possible */
	for(i=0; i+63<inputLen; i+=64) {
                for(var j=index; j<64; j++)
			buffer[j] = data.charCodeAt(curpos++);
		sha256_transform();
		index = 0;
	}

	/* Buffer remaining input */
	for(var j=0; j<remainder; j++)
		buffer[j] = data.charCodeAt(curpos++);
}

/* Finish the computation by operations such as padding */
function sha256_final() {
	var index = ((count[0] >> 3) & 0x3f);
        buffer[index++] = 0x80;
        if(index <= 56) {
		for(var i=index; i<56; i++)
			buffer[i] = 0;
        } else {
		for(var i=index; i<64; i++)
			buffer[i] = 0;
                sha256_transform();
                for(var i=0; i<56; i++)
			buffer[i] = 0;
	}
        buffer[56] = (count[1] >>> 24) & 0xff;
        buffer[57] = (count[1] >>> 16) & 0xff;
        buffer[58] = (count[1] >>> 8) & 0xff;
        buffer[59] = count[1] & 0xff;
        buffer[60] = (count[0] >>> 24) & 0xff;
        buffer[61] = (count[0] >>> 16) & 0xff;
        buffer[62] = (count[0] >>> 8) & 0xff;
        buffer[63] = count[0] & 0xff;
        sha256_transform();
}

/* Split the internal hash values into an array of bytes */
function sha256_encode_bytes() {
        var j=0;
        var output = new Array(32);
	for(var i=0; i<8; i++) {
		output[j++] = ((ihash[i] >>> 24) & 0xff);
		output[j++] = ((ihash[i] >>> 16) & 0xff);
		output[j++] = ((ihash[i] >>> 8) & 0xff);
		output[j++] = (ihash[i] & 0xff);
	}
	return output;
}

/* Get the internal hash as a hex string */
function sha256_encode_hex() {
	var output = new String();
	for(var i=0; i<8; i++) {
		for(var j=28; j>=0; j-=4)
			output += sha256_hex_digits.charAt((ihash[i] >>> j) & 0x0f);
	}
	return output;
}

/* Main function: returns a hex string representing the SHA256 value of the
given data */
function sha256_digest(data) {
	sha256_init();
	sha256_update(data, data.length);
	sha256_final();
        return sha256_encode_hex();
}

/* test if the JS-interpreter is working properly */
function sha256_self_test()
{
	return sha256_digest("message digest") ==
"f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650";
}


(function (request, xapi) {
    'use strict';
    //Builds query parameters for a GET request
    function buildUrl(url, parameters) {
        var params = [];
        for (var key in parameters) {
            var value = parameters[key];
            if (value && parameters.hasOwnProperty(key) && typeof value !== 'function') {
                //qs += encodeURIComponent(key) + '=' + encodeURIComponent(typeof value === 'object' ? JSON.stringify(value) : value) + '&';
                params.push(encodeURIComponent(key) + '=' + encodeURIComponent(typeof value === 'object' ? JSON.stringify(value) : value));
            }
        }

        if (params.length > 0) {
            url = url + '?' + params.join('&');
        }

        return url;
    }

    /**
     * Calls ajax with the correct parameters
     * @param object
     */
    function ajaxSetup(options) {
        //Require URL
        if (typeof options.url !== 'string') {
            throw new Error('Please provide a url');
        }

        //Build query string for GET requests
        if (options.params && typeof options.params === 'object') {
            options.url = buildUrl(options.url, options.params);
        }

        //All none GET requests send JSON data
        if (typeof options.data === 'object' && options.type.toUpperCase() !== 'GET') {
            options.data = JSON.stringify(options.data);
        }

        if (options.files && options.files.length > 0) {
            var boundary = '-------314159265358979323846';
            var delimiter = '\r\n--' + boundary + '\r\n';
            var closeDelim = '\r\n--' + boundary + '--';
            options.contentType = 'multipart/mixed; boundary="' + boundary + '"';

            //Add statement(s) to request
            var multipartRequestBody = delimiter + 'Content-Type: application/json\r\n\r\n' + options.data;

            //Add file(s) to request
            var fileCount = options.files.length;
            var filesProcessed = 0;
            for (var i = 0; i < fileCount; i++) {
                var reader = new FileReader();

                if(reader.readAsBinaryString){
                    reader.readAsBinaryString(options.files[i]);
                    reader.onload = (function (i) {
                        return function (e) {
                            var contentType = options.files[i].type || 'text/plain';
                            multipartRequestBody +=
                                delimiter +
                                'Content-Type: ' + contentType + '\r\n' +
                                'Content-Disposition: attachment; filename="' + options.files[i].name + '"\r\n' +
                                'Content-Transfer-Encoding: binary\r\n' +
                                'X-Experience-API-Hash: ' + sha256_digest(e.target.result) + '\r\n' +
                                '\r\n' +
                                e.target.result;

                            filesProcessed++;

                            //Needed to handle the asynchronous processing of files
                            if (filesProcessed === fileCount) {
                                multipartRequestBody += closeDelim;
                                options.data = multipartRequestBody;
                                ajax(options.url, options.type ? options.type : 'GET', options.data || '', options.contentType, options.headers, options.success, options.error);
                            }
                        };
                    })(i);
                    } else if(reader.readAsArrayBuffer){
                        reader.readAsArrayBuffer(options.files[i]);
                        reader.onload = (function (i) {
                            return function (e) {
                                var content = String.fromCharCode.apply(null, new Uint8Array(e.target.result));

                                var contentType = options.files[i].type || 'text/plain';
                                multipartRequestBody +=
                                    delimiter +
                                    'Content-Type: ' + contentType + '\r\n' +
                                    'Content-Disposition: attachment; filename="' + options.files[i].name + '"\r\n' +
                                    'Content-Transfer-Encoding: binary\r\n' +
                                    'X-Experience-API-Hash: ' + sha256_digest(content) + '\r\n' +
                                    '\r\n' +
                                    content;

                                filesProcessed++;

                                //Needed to handle the asynchronous processing of files
                                if (filesProcessed === fileCount) {
                                    multipartRequestBody += closeDelim;
                                    options.data = multipartRequestBody;
                                    ajax(options.url, options.type ? options.type : 'GET', options.data || '', options.contentType, options.headers, options.success, options.error);
                                }
                            };
                        })(i);
                }
            }
        } else {
            ajax(options.url, options.type ? options.type : 'GET', options.data || '', options.contentType, options.headers, options.success, options.error);
        }
    }

    /**
     * Creates a crossbrowser CORS request
     * @param {string} url
     * @param {string} [type=GET] - GET (Default), POST, PUT, DELETE
     * @param {object|string} [data]
     * @param {string} [contentType]
     * @param {function} [callback] - function to call on success
     * @param {function} [errback] - function to call on error
     */
    function ajax(url, type, data, contentType, headers, callback, errback) {
        //Check for browser support
        var req;
        if (XMLHttpRequest) {
            req = new XMLHttpRequest();
        } else if (XDomainRequest) {
            req = new XDomainRequest();
        } else {
            typeof errback === 'function' && errback('CORS not supported');
            return false;
        }

        req.open(type, url, true);
        if (typeof contentType === 'string') {
            req.setRequestHeader('Content-type', contentType);
        } else if (type.toUpperCase() !== 'GET') {
            req.setRequestHeader('Content-type', 'application/json');
        }

        req.setRequestHeader('X-Experience-API-Version', xapi.version);

        // Set additional headers
        if (headers) {
            var keys = Object.keys(headers);
            keys.forEach(function (key) {
                var headerValue = headers[key];
                req.setRequestHeader(key, headerValue);
            });
        }

        req.onerror = function (err) {
            if (typeof errback === 'function') {
                errback(req, req.statusText, err);
            }
        };

        var responded = false;
        req.onload = function () {
            if (req.readyState === 4 && !responded) {
                if (req.status >= 200 && req.status < 400) {
                    if (typeof callback === 'function') {
                        callback(req.getAllResponseHeaders().toLowerCase().search('application/json') === -1 ? req.response : JSON.parse(req.response || req.responseText), req.statusText, req);
                    }
                } else {
                    if (typeof errback === 'function') {
                        errback(req, req.statusText, req.getAllResponseHeaders().toLowerCase().search('application/json') === -1 ? req.response : JSON.parse(req.response || req.responseText));
                    }
                }
                responded = true;
            }
        };

        // Internet Explorer 8 doesn't fire `onload` when it has finished loading, instead it fires `onreadystatechange`.
        req.onreadystatechange = req.onload;

        req.send(data);
    }

    request.ajax = ajaxSetup;

}(this.request = {}, 'xapi' in this ? this.xapi : this.xapi = {}));
(function (exports) {
    'use strict';
    //test that the argument is not undefined or null and that it matches
    RegExp.prototype.definedTest = function (arg) {
        return !(arg === undefined || arg === null) && this.test(arg);
    };

    exports.mailTo = /^mailto:(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/i;
    var uri = new RegExp(
        '^' +
            // protocol identifier
        '(?:(?:https?|ftp)://)' +
            // user:pass authentication
        '(?:\\S+(?::\\S*)?@)?' +
        '(?:' +
            // IP address exclusion
            // private & local networks
        '(?!10(?:\\.\\d{1,3}){3})' +
        '(?!127(?:\\.\\d{1,3}){3})' +
        '(?!169\\.254(?:\\.\\d{1,3}){2})' +
        '(?!192\\.168(?:\\.\\d{1,3}){2})' +
        '(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})' +
            // IP address dotted notation octets
            // excludes loopback network 0.0.0.0
            // excludes reserved space >= 224.0.0.0
            // excludes network & broacast addresses
            // (first & last IP address of each class)
        '(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])' +
        '(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}' +
        '(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))' +
        '|' +
            // host name
        '(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)' +
            // domain name
        '(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*' +
            // TLD identifier
        '(?:\\.(?:[a-z\\u00a1-\\uffff]{2,}))' +
        ')' +
            // port number
        '(?::\\d{2,5})?' +
            // resource path
        '(?:/[^\\s]*)?' +
        '$', 'i'
    );

    //https://gist.github.com/ebouchut/6699245
    var extLang = '([A-Za-z]{3}(-[A-Za-z]{3}){0,2})';
    var language = '(([a-zA-Z]{2,3}(-' + extLang + ')?)|([a-zA-Z]{5,8}))';
    var script = '([A-Za-z]{4})';
    var region = '([A-Za-z]{2}|\\d{3})';
    var variant = '([A-Za-z0-9]{5,8}|(\\d[A-Z-a-z0-9]{3}))';
    var singleton = '(\\d|[A-W]|[Y-Z]|[a-w]|[y-z])';
    var extension = '(' + singleton + '(-[A-Za-z0-9]{2,8})+)';
    var privateUse = '(x(-[A-Za-z0-9]{1,8})+)';
    var langTag = language + '(-' + script + ')?(-' + region + ')?(-' + variant + ')*(-' + extension + ')*(-' + privateUse + ')?';
    var irregular = '((en-GB-oed)|(i-ami)|(i-bnn)|(i-default)|(i-enochian)|(i-hak)|(i-klingon)|(i-lux)|(i-mingo)|(i-navajo)|(i-pwn)|(i-tao)|(i-tay)|(i-tsu)|(sgn-BE-FR)|(sgn-BE-NL)|(sgn-CH-DE))';
    var regular = '((art-lojban)|(cel-gaulish)|(no-bok)|(no-nyn)|(zh-guoyu)|(zh-hakka)|(zh-min)|(zh-min-nan)|(zh-xiang))';
    var grandFathered = '(' + irregular + '|' + regular + ')';

    exports.uri = uri;
    exports.uuid = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
    exports.agentOrGroup = new RegExp('^(Agent|Group)$');
    exports.agentOrGroupCase = new RegExp('^(agent|group)$');
    exports.activity = new RegExp('^(Activity)$');
    exports.activityCase = new RegExp('^(activity)$');
    exports.boolean = /^(true|false)$/i;
    exports.booleanFalse = /^(false)$/i;
    exports.booleanTrue = /^(true)$/i;
    exports.languageMap = new RegExp('(^' + privateUse + '$)' + '|' + '(^' + grandFathered + '$)' + '|' + '(^' + langTag + '$)');
    exports.activityObjectType = /^(Activity)$/;
    exports.activityObjectTypeCase = /^(activity)$/;
    exports.objectObjectType = /^(Activity|Agent|Group|SubStatement|StatementRef)$/;
    exports.objectObjectTypeCase = /^(activity|agent|group|substatement|statementref)$/;
    exports.agent = /^Agent$/;
    exports.agentCase = /^agent$/;
    exports.group = /^Group$/;
    exports.groupCase = /^group$/;
    exports.interactionType = RegExp('^(choice|sequencing|likert|matching|performance|true-false|long-fill-in|fill-in|numeric|other)$', 'i');
    exports.interactionActivityType = RegExp('^http://adlnet.gov/expapi/activities/cmi.interaction/$', 'i');
    exports.objectTypeStatementRef = /^(StatementRef)$/;
    exports.objectTypeStatementRefCase = /^(statementref)$/;
    exports.version10 = /^1.0.[0-9]+$|^1.0$/i;
    //exports.ISO8601 = /^(\d{4}\-\d\d\-\d\d([tT][\d:\.]*)?)([zZ]|([+\-])(\d\d):?(\d\d))?$/;
    exports.extensions = {
        test: function (obj) {
            for (var key in obj) {
                if (!uri.test(key)) {
                    return false;
                }
            }
            return true;
        }
    };
    exports.substatement = new RegExp('^(SubStatement)$');
    exports.substatementCase = new RegExp('^(substatement)$');
    exports.statementref = new RegExp('^(StatementRef)$');
    exports.statementrefCase = new RegExp('^(statementref)$');
    exports.substatementType = {
        test: function (obj) {
            var validSubStatement = !(obj.id && obj.stored && obj.version && obj.authority),
                hasSubStatement = 'object' in this && 'objectType' in this.object && this.object.objectType && exports.substatement.test(this.object.objectType);
            return !hasSubStatement && validSubStatement;
        }
    };
//exports.interactionComponentId = /\s/
    exports.isoDate = /^([\+-]?\d{4}(?!\d{2}\b))((-?)((0[1-9]|1[0-2])(\3([12]\d|0[1-9]|3[01]))?|W([0-4]\d|5[0-2])(-?[1-7])?|(00[1-9]|0[1-9]\d|[12]\d{2}|3([0-5]\d|6[1-6])))([T\s]((([01]\d|2[0-3])((:?)[0-5]\d)?|24\:?00)([\.,]\d+(?!:))?)?(\17[0-5]\d([\.,]\d+)?)?([zZ]|([\+-])([01]\d|2[0-3]):?([0-5]\d)?)?)?)?$/;
    //https://github.com/moment/moment, https://github.com/moment/moment/pull/1113
    exports.isoDuration = /^(-)?P(?:(?:([0-9,.]*)Y)?(?:([0-9,.]*)M)?(?:([0-9,.]*)D)?(?:T(?:([0-9,.]*)H)?(?:([0-9,.]*)M)?(?:([0-9,.]*)S)?)?|([0-9,.]*)W)$/;
}((typeof module !== 'undefined' && module.exports) ? exports : ('typeValidator' in this ? this.typeValidator : this.typeValidator = {})));
//     uuid.js
//
//     Copyright (c) 2010-2012 Robert Kieffer
//     MIT License - http://opensource.org/licenses/mit-license.php

(function() {
  var _global = this;

  // Unique ID creation requires a high quality random # generator.  We feature
  // detect to determine the best RNG source, normalizing to a function that
  // returns 128-bits of randomness, since that's what's usually required
  var _rng;

  // Node.js crypto-based RNG - http://nodejs.org/docs/v0.6.2/api/crypto.html
  //
  // Moderately fast, high quality
  if (typeof(require) == 'function') {
    try {
      var _rb = require('crypto').randomBytes;
      _rng = _rb && function() {return _rb(16);};
    } catch(e) {}
  }

  if (!_rng && _global.crypto && crypto.getRandomValues) {
    // WHATWG crypto-based RNG - http://wiki.whatwg.org/wiki/Crypto
    //
    // Moderately fast, high quality
    var _rnds8 = new Uint8Array(16);
    _rng = function whatwgRNG() {
      crypto.getRandomValues(_rnds8);
      return _rnds8;
    };
  }

  if (!_rng) {
    // Math.random()-based (RNG)
    //
    // If all else fails, use Math.random().  It's fast, but is of unspecified
    // quality.
    var  _rnds = new Array(16);
    _rng = function() {
      for (var i = 0, r; i < 16; i++) {
        if ((i & 0x03) === 0) r = Math.random() * 0x100000000;
        _rnds[i] = r >>> ((i & 0x03) << 3) & 0xff;
      }

      return _rnds;
    };
  }

  // Buffer class to use
  var BufferClass = typeof(Buffer) == 'function' ? Buffer : Array;

  // Maps for number <-> hex string conversion
  var _byteToHex = [];
  var _hexToByte = {};
  for (var i = 0; i < 256; i++) {
    _byteToHex[i] = (i + 0x100).toString(16).substr(1);
    _hexToByte[_byteToHex[i]] = i;
  }

  // **`parse()` - Parse a UUID into it's component bytes**
  function parse(s, buf, offset) {
    var i = (buf && offset) || 0, ii = 0;

    buf = buf || [];
    s.toLowerCase().replace(/[0-9a-f]{2}/g, function(oct) {
      if (ii < 16) { // Don't overflow!
        buf[i + ii++] = _hexToByte[oct];
      }
    });

    // Zero out remaining bytes if string was short
    while (ii < 16) {
      buf[i + ii++] = 0;
    }

    return buf;
  }

  // **`unparse()` - Convert UUID byte array (ala parse()) into a string**
  function unparse(buf, offset) {
    var i = offset || 0, bth = _byteToHex;
    return  bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] + '-' +
            bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]] +
            bth[buf[i++]] + bth[buf[i++]];
  }

  // **`v1()` - Generate time-based UUID**
  //
  // Inspired by https://github.com/LiosK/UUID.js
  // and http://docs.python.org/library/uuid.html

  // random #'s we need to init node and clockseq
  var _seedBytes = _rng();

  // Per 4.5, create and 48-bit node id, (47 random bits + multicast bit = 1)
  var _nodeId = [
    _seedBytes[0] | 0x01,
    _seedBytes[1], _seedBytes[2], _seedBytes[3], _seedBytes[4], _seedBytes[5]
  ];

  // Per 4.2.2, randomize (14 bit) clockseq
  var _clockseq = (_seedBytes[6] << 8 | _seedBytes[7]) & 0x3fff;

  // Previous uuid creation time
  var _lastMSecs = 0, _lastNSecs = 0;

  // See https://github.com/broofa/node-uuid for API details
  function v1(options, buf, offset) {
    var i = buf && offset || 0;
    var b = buf || [];

    options = options || {};

    var clockseq = options.clockseq != null ? options.clockseq : _clockseq;

    // UUID timestamps are 100 nano-second units since the Gregorian epoch,
    // (1582-10-15 00:00).  JSNumbers aren't precise enough for this, so
    // time is handled internally as 'msecs' (integer milliseconds) and 'nsecs'
    // (100-nanoseconds offset from msecs) since unix epoch, 1970-01-01 00:00.
    var msecs = options.msecs != null ? options.msecs : new Date().getTime();

    // Per 4.2.1.2, use count of uuid's generated during the current clock
    // cycle to simulate higher resolution clock
    var nsecs = options.nsecs != null ? options.nsecs : _lastNSecs + 1;

    // Time since last uuid creation (in msecs)
    var dt = (msecs - _lastMSecs) + (nsecs - _lastNSecs)/10000;

    // Per 4.2.1.2, Bump clockseq on clock regression
    if (dt < 0 && options.clockseq == null) {
      clockseq = clockseq + 1 & 0x3fff;
    }

    // Reset nsecs if clock regresses (new clockseq) or we've moved onto a new
    // time interval
    if ((dt < 0 || msecs > _lastMSecs) && options.nsecs == null) {
      nsecs = 0;
    }

    // Per 4.2.1.2 Throw error if too many uuids are requested
    if (nsecs >= 10000) {
      throw new Error('uuid.v1(): Can\'t create more than 10M uuids/sec');
    }

    _lastMSecs = msecs;
    _lastNSecs = nsecs;
    _clockseq = clockseq;

    // Per 4.1.4 - Convert from unix epoch to Gregorian epoch
    msecs += 12219292800000;

    // `time_low`
    var tl = ((msecs & 0xfffffff) * 10000 + nsecs) % 0x100000000;
    b[i++] = tl >>> 24 & 0xff;
    b[i++] = tl >>> 16 & 0xff;
    b[i++] = tl >>> 8 & 0xff;
    b[i++] = tl & 0xff;

    // `time_mid`
    var tmh = (msecs / 0x100000000 * 10000) & 0xfffffff;
    b[i++] = tmh >>> 8 & 0xff;
    b[i++] = tmh & 0xff;

    // `time_high_and_version`
    b[i++] = tmh >>> 24 & 0xf | 0x10; // include version
    b[i++] = tmh >>> 16 & 0xff;

    // `clock_seq_hi_and_reserved` (Per 4.2.2 - include variant)
    b[i++] = clockseq >>> 8 | 0x80;

    // `clock_seq_low`
    b[i++] = clockseq & 0xff;

    // `node`
    var node = options.node || _nodeId;
    for (var n = 0; n < 6; n++) {
      b[i + n] = node[n];
    }

    return buf ? buf : unparse(b);
  }

  // **`v4()` - Generate random UUID**

  // See https://github.com/broofa/node-uuid for API details
  function v4(options, buf, offset) {
    // Deprecated - 'format' argument, as supported in v1.2
    var i = buf && offset || 0;

    if (typeof(options) == 'string') {
      buf = options == 'binary' ? new BufferClass(16) : null;
      options = null;
    }
    options = options || {};

    var rnds = options.random || (options.rng || _rng)();

    // Per 4.4, set bits for version and `clock_seq_hi_and_reserved`
    rnds[6] = (rnds[6] & 0x0f) | 0x40;
    rnds[8] = (rnds[8] & 0x3f) | 0x80;

    // Copy bytes to buffer, if provided
    if (buf) {
      for (var ii = 0; ii < 16; ii++) {
        buf[i + ii] = rnds[ii];
      }
    }

    return buf || unparse(rnds);
  }

  // Export public API
  var uuid = v4;
  uuid.v1 = v1;
  uuid.v4 = v4;
  uuid.parse = parse;
  uuid.unparse = unparse;
  uuid.BufferClass = BufferClass;

  if (typeof define === 'function' && define.amd) {
    // Publish as AMD module
    define(function() {return uuid;});
  } else if (typeof(module) != 'undefined' && module.exports) {
    // Publish as node.js module
    module.exports = uuid;
  } else {
    // Publish as global (in browsers)
    var _previousRoot = _global.uuid;

    // **`noConflict()` - (browser only) to reset global 'uuid' var**
    uuid.noConflict = function() {
      _global.uuid = _previousRoot;
      return uuid;
    };

    _global.uuid = uuid;
  }
}).call(this);
!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var f;"undefined"!=typeof window?f=window:"undefined"!=typeof global?f=global:"undefined"!=typeof self&&(f=self),f.io=e()}}(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(_dereq_,module,exports){

module.exports = _dereq_('./lib/');

},{"./lib/":2}],2:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var url = _dereq_('./url');
var parser = _dereq_('socket.io-parser');
var Manager = _dereq_('./manager');
var debug = _dereq_('debug')('socket.io-client');

/**
 * Module exports.
 */

module.exports = exports = lookup;

/**
 * Managers cache.
 */

var cache = exports.managers = {};

/**
 * Looks up an existing `Manager` for multiplexing.
 * If the user summons:
 *
 *   `io('http://localhost/a');`
 *   `io('http://localhost/b');`
 *
 * We reuse the existing instance based on same scheme/port/host,
 * and we initialize sockets for each namespace.
 *
 * @api public
 */

function lookup(uri, opts) {
  if (typeof uri == 'object') {
    opts = uri;
    uri = undefined;
  }

  opts = opts || {};

  var parsed = url(uri);
  var source = parsed.source;
  var id = parsed.id;
  var io;

  if (opts.forceNew || opts['force new connection'] || false === opts.multiplex) {
    debug('ignoring socket cache for %s', source);
    io = Manager(source, opts);
  } else {
    if (!cache[id]) {
      debug('new io instance for %s', source);
      cache[id] = Manager(source, opts);
    }
    io = cache[id];
  }

  return io.socket(parsed.path);
}

/**
 * Protocol version.
 *
 * @api public
 */

exports.protocol = parser.protocol;

/**
 * `connect`.
 *
 * @param {String} uri
 * @api public
 */

exports.connect = lookup;

/**
 * Expose constructors for standalone build.
 *
 * @api public
 */

exports.Manager = _dereq_('./manager');
exports.Socket = _dereq_('./socket');

},{"./manager":3,"./socket":5,"./url":6,"debug":10,"socket.io-parser":46}],3:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var url = _dereq_('./url');
var eio = _dereq_('engine.io-client');
var Socket = _dereq_('./socket');
var Emitter = _dereq_('component-emitter');
var parser = _dereq_('socket.io-parser');
var on = _dereq_('./on');
var bind = _dereq_('component-bind');
var object = _dereq_('object-component');
var debug = _dereq_('debug')('socket.io-client:manager');
var indexOf = _dereq_('indexof');
var Backoff = _dereq_('backo2');

/**
 * Module exports
 */

module.exports = Manager;

/**
 * `Manager` constructor.
 *
 * @param {String} engine instance or engine uri/opts
 * @param {Object} options
 * @api public
 */

function Manager(uri, opts){
  if (!(this instanceof Manager)) return new Manager(uri, opts);
  if (uri && ('object' == typeof uri)) {
    opts = uri;
    uri = undefined;
  }
  opts = opts || {};

  opts.path = opts.path || '/socket.io';
  this.nsps = {};
  this.subs = [];
  this.opts = opts;
  this.reconnection(opts.reconnection !== false);
  this.reconnectionAttempts(opts.reconnectionAttempts || Infinity);
  this.reconnectionDelay(opts.reconnectionDelay || 1000);
  this.reconnectionDelayMax(opts.reconnectionDelayMax || 5000);
  this.randomizationFactor(opts.randomizationFactor || 0.5);
  this.backoff = new Backoff({
    min: this.reconnectionDelay(),
    max: this.reconnectionDelayMax(),
    jitter: this.randomizationFactor()
  });
  this.timeout(null == opts.timeout ? 20000 : opts.timeout);
  this.readyState = 'closed';
  this.uri = uri;
  this.connected = [];
  this.encoding = false;
  this.packetBuffer = [];
  this.encoder = new parser.Encoder();
  this.decoder = new parser.Decoder();
  this.autoConnect = opts.autoConnect !== false;
  if (this.autoConnect) this.open();
}

/**
 * Propagate given event to sockets and emit on `this`
 *
 * @api private
 */

Manager.prototype.emitAll = function() {
  this.emit.apply(this, arguments);
  for (var nsp in this.nsps) {
    this.nsps[nsp].emit.apply(this.nsps[nsp], arguments);
  }
};

/**
 * Update `socket.id` of all sockets
 *
 * @api private
 */

Manager.prototype.updateSocketIds = function(){
  for (var nsp in this.nsps) {
    this.nsps[nsp].id = this.engine.id;
  }
};

/**
 * Mix in `Emitter`.
 */

Emitter(Manager.prototype);

/**
 * Sets the `reconnection` config.
 *
 * @param {Boolean} true/false if it should automatically reconnect
 * @return {Manager} self or value
 * @api public
 */

Manager.prototype.reconnection = function(v){
  if (!arguments.length) return this._reconnection;
  this._reconnection = !!v;
  return this;
};

/**
 * Sets the reconnection attempts config.
 *
 * @param {Number} max reconnection attempts before giving up
 * @return {Manager} self or value
 * @api public
 */

Manager.prototype.reconnectionAttempts = function(v){
  if (!arguments.length) return this._reconnectionAttempts;
  this._reconnectionAttempts = v;
  return this;
};

/**
 * Sets the delay between reconnections.
 *
 * @param {Number} delay
 * @return {Manager} self or value
 * @api public
 */

Manager.prototype.reconnectionDelay = function(v){
  if (!arguments.length) return this._reconnectionDelay;
  this._reconnectionDelay = v;
  this.backoff && this.backoff.setMin(v);
  return this;
};

Manager.prototype.randomizationFactor = function(v){
  if (!arguments.length) return this._randomizationFactor;
  this._randomizationFactor = v;
  this.backoff && this.backoff.setJitter(v);
  return this;
};

/**
 * Sets the maximum delay between reconnections.
 *
 * @param {Number} delay
 * @return {Manager} self or value
 * @api public
 */

Manager.prototype.reconnectionDelayMax = function(v){
  if (!arguments.length) return this._reconnectionDelayMax;
  this._reconnectionDelayMax = v;
  this.backoff && this.backoff.setMax(v);
  return this;
};

/**
 * Sets the connection timeout. `false` to disable
 *
 * @return {Manager} self or value
 * @api public
 */

Manager.prototype.timeout = function(v){
  if (!arguments.length) return this._timeout;
  this._timeout = v;
  return this;
};

/**
 * Starts trying to reconnect if reconnection is enabled and we have not
 * started reconnecting yet
 *
 * @api private
 */

Manager.prototype.maybeReconnectOnOpen = function() {
  // Only try to reconnect if it's the first time we're connecting
  if (!this.reconnecting && this._reconnection && this.backoff.attempts === 0) {
    // keeps reconnection from firing twice for the same reconnection loop
    this.reconnect();
  }
};


/**
 * Sets the current transport `socket`.
 *
 * @param {Function} optional, callback
 * @return {Manager} self
 * @api public
 */

Manager.prototype.open =
Manager.prototype.connect = function(fn){
  debug('readyState %s', this.readyState);
  if (~this.readyState.indexOf('open')) return this;

  debug('opening %s', this.uri);
  this.engine = eio(this.uri, this.opts);
  var socket = this.engine;
  var self = this;
  this.readyState = 'opening';
  this.skipReconnect = false;

  // emit `open`
  var openSub = on(socket, 'open', function() {
    self.onopen();
    fn && fn();
  });

  // emit `connect_error`
  var errorSub = on(socket, 'error', function(data){
    debug('connect_error');
    self.cleanup();
    self.readyState = 'closed';
    self.emitAll('connect_error', data);
    if (fn) {
      var err = new Error('Connection error');
      err.data = data;
      fn(err);
    } else {
      // Only do this if there is no fn to handle the error
      self.maybeReconnectOnOpen();
    }
  });

  // emit `connect_timeout`
  if (false !== this._timeout) {
    var timeout = this._timeout;
    debug('connect attempt will timeout after %d', timeout);

    // set timer
    var timer = setTimeout(function(){
      debug('connect attempt timed out after %d', timeout);
      openSub.destroy();
      socket.close();
      socket.emit('error', 'timeout');
      self.emitAll('connect_timeout', timeout);
    }, timeout);

    this.subs.push({
      destroy: function(){
        clearTimeout(timer);
      }
    });
  }

  this.subs.push(openSub);
  this.subs.push(errorSub);

  return this;
};

/**
 * Called upon transport open.
 *
 * @api private
 */

Manager.prototype.onopen = function(){
  debug('open');

  // clear old subs
  this.cleanup();

  // mark as open
  this.readyState = 'open';
  this.emit('open');

  // add new subs
  var socket = this.engine;
  this.subs.push(on(socket, 'data', bind(this, 'ondata')));
  this.subs.push(on(this.decoder, 'decoded', bind(this, 'ondecoded')));
  this.subs.push(on(socket, 'error', bind(this, 'onerror')));
  this.subs.push(on(socket, 'close', bind(this, 'onclose')));
};

/**
 * Called with data.
 *
 * @api private
 */

Manager.prototype.ondata = function(data){
  this.decoder.add(data);
};

/**
 * Called when parser fully decodes a packet.
 *
 * @api private
 */

Manager.prototype.ondecoded = function(packet) {
  this.emit('packet', packet);
};

/**
 * Called upon socket error.
 *
 * @api private
 */

Manager.prototype.onerror = function(err){
  debug('error', err);
  this.emitAll('error', err);
};

/**
 * Creates a new socket for the given `nsp`.
 *
 * @return {Socket}
 * @api public
 */

Manager.prototype.socket = function(nsp){
  var socket = this.nsps[nsp];
  if (!socket) {
    socket = new Socket(this, nsp);
    this.nsps[nsp] = socket;
    var self = this;
    socket.on('connect', function(){
      socket.id = self.engine.id;
      if (!~indexOf(self.connected, socket)) {
        self.connected.push(socket);
      }
    });
  }
  return socket;
};

/**
 * Called upon a socket close.
 *
 * @param {Socket} socket
 */

Manager.prototype.destroy = function(socket){
  var index = indexOf(this.connected, socket);
  if (~index) this.connected.splice(index, 1);
  if (this.connected.length) return;

  this.close();
};

/**
 * Writes a packet.
 *
 * @param {Object} packet
 * @api private
 */

Manager.prototype.packet = function(packet){
  debug('writing packet %j', packet);
  var self = this;

  if (!self.encoding) {
    // encode, then write to engine with result
    self.encoding = true;
    this.encoder.encode(packet, function(encodedPackets) {
      for (var i = 0; i < encodedPackets.length; i++) {
        self.engine.write(encodedPackets[i]);
      }
      self.encoding = false;
      self.processPacketQueue();
    });
  } else { // add packet to the queue
    self.packetBuffer.push(packet);
  }
};

/**
 * If packet buffer is non-empty, begins encoding the
 * next packet in line.
 *
 * @api private
 */

Manager.prototype.processPacketQueue = function() {
  if (this.packetBuffer.length > 0 && !this.encoding) {
    var pack = this.packetBuffer.shift();
    this.packet(pack);
  }
};

/**
 * Clean up transport subscriptions and packet buffer.
 *
 * @api private
 */

Manager.prototype.cleanup = function(){
  var sub;
  while (sub = this.subs.shift()) sub.destroy();

  this.packetBuffer = [];
  this.encoding = false;

  this.decoder.destroy();
};

/**
 * Close the current socket.
 *
 * @api private
 */

Manager.prototype.close =
Manager.prototype.disconnect = function(){
  this.skipReconnect = true;
  this.backoff.reset();
  this.readyState = 'closed';
  this.engine && this.engine.close();
};

/**
 * Called upon engine close.
 *
 * @api private
 */

Manager.prototype.onclose = function(reason){
  debug('close');
  this.cleanup();
  this.backoff.reset();
  this.readyState = 'closed';
  this.emit('close', reason);
  if (this._reconnection && !this.skipReconnect) {
    this.reconnect();
  }
};

/**
 * Attempt a reconnection.
 *
 * @api private
 */

Manager.prototype.reconnect = function(){
  if (this.reconnecting || this.skipReconnect) return this;

  var self = this;

  if (this.backoff.attempts >= this._reconnectionAttempts) {
    debug('reconnect failed');
    this.backoff.reset();
    this.emitAll('reconnect_failed');
    this.reconnecting = false;
  } else {
    var delay = this.backoff.duration();
    debug('will wait %dms before reconnect attempt', delay);

    this.reconnecting = true;
    var timer = setTimeout(function(){
      if (self.skipReconnect) return;

      debug('attempting reconnect');
      self.emitAll('reconnect_attempt', self.backoff.attempts);
      self.emitAll('reconnecting', self.backoff.attempts);

      // check again for the case socket closed in above events
      if (self.skipReconnect) return;

      self.open(function(err){
        if (err) {
          debug('reconnect attempt error');
          self.reconnecting = false;
          self.reconnect();
          self.emitAll('reconnect_error', err.data);
        } else {
          debug('reconnect success');
          self.onreconnect();
        }
      });
    }, delay);

    this.subs.push({
      destroy: function(){
        clearTimeout(timer);
      }
    });
  }
};

/**
 * Called upon successful reconnect.
 *
 * @api private
 */

Manager.prototype.onreconnect = function(){
  var attempt = this.backoff.attempts;
  this.reconnecting = false;
  this.backoff.reset();
  this.updateSocketIds();
  this.emitAll('reconnect', attempt);
};

},{"./on":4,"./socket":5,"./url":6,"backo2":7,"component-bind":8,"component-emitter":9,"debug":10,"engine.io-client":11,"indexof":42,"object-component":43,"socket.io-parser":46}],4:[function(_dereq_,module,exports){

/**
 * Module exports.
 */

module.exports = on;

/**
 * Helper for subscriptions.
 *
 * @param {Object|EventEmitter} obj with `Emitter` mixin or `EventEmitter`
 * @param {String} event name
 * @param {Function} callback
 * @api public
 */

function on(obj, ev, fn) {
  obj.on(ev, fn);
  return {
    destroy: function(){
      obj.removeListener(ev, fn);
    }
  };
}

},{}],5:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var parser = _dereq_('socket.io-parser');
var Emitter = _dereq_('component-emitter');
var toArray = _dereq_('to-array');
var on = _dereq_('./on');
var bind = _dereq_('component-bind');
var debug = _dereq_('debug')('socket.io-client:socket');
var hasBin = _dereq_('has-binary');

/**
 * Module exports.
 */

module.exports = exports = Socket;

/**
 * Internal events (blacklisted).
 * These events can't be emitted by the user.
 *
 * @api private
 */

var events = {
  connect: 1,
  connect_error: 1,
  connect_timeout: 1,
  disconnect: 1,
  error: 1,
  reconnect: 1,
  reconnect_attempt: 1,
  reconnect_failed: 1,
  reconnect_error: 1,
  reconnecting: 1
};

/**
 * Shortcut to `Emitter#emit`.
 */

var emit = Emitter.prototype.emit;

/**
 * `Socket` constructor.
 *
 * @api public
 */

function Socket(io, nsp){
  this.io = io;
  this.nsp = nsp;
  this.json = this; // compat
  this.ids = 0;
  this.acks = {};
  if (this.io.autoConnect) this.open();
  this.receiveBuffer = [];
  this.sendBuffer = [];
  this.connected = false;
  this.disconnected = true;
}

/**
 * Mix in `Emitter`.
 */

Emitter(Socket.prototype);

/**
 * Subscribe to open, close and packet events
 *
 * @api private
 */

Socket.prototype.subEvents = function() {
  if (this.subs) return;

  var io = this.io;
  this.subs = [
    on(io, 'open', bind(this, 'onopen')),
    on(io, 'packet', bind(this, 'onpacket')),
    on(io, 'close', bind(this, 'onclose'))
  ];
};

/**
 * "Opens" the socket.
 *
 * @api public
 */

Socket.prototype.open =
Socket.prototype.connect = function(){
  if (this.connected) return this;

  this.subEvents();
  this.io.open(); // ensure open
  if ('open' == this.io.readyState) this.onopen();
  return this;
};

/**
 * Sends a `message` event.
 *
 * @return {Socket} self
 * @api public
 */

Socket.prototype.send = function(){
  var args = toArray(arguments);
  args.unshift('message');
  this.emit.apply(this, args);
  return this;
};

/**
 * Override `emit`.
 * If the event is in `events`, it's emitted normally.
 *
 * @param {String} event name
 * @return {Socket} self
 * @api public
 */

Socket.prototype.emit = function(ev){
  if (events.hasOwnProperty(ev)) {
    emit.apply(this, arguments);
    return this;
  }

  var args = toArray(arguments);
  var parserType = parser.EVENT; // default
  if (hasBin(args)) { parserType = parser.BINARY_EVENT; } // binary
  var packet = { type: parserType, data: args };

  // event ack callback
  if ('function' == typeof args[args.length - 1]) {
    debug('emitting packet with ack id %d', this.ids);
    this.acks[this.ids] = args.pop();
    packet.id = this.ids++;
  }

  if (this.connected) {
    this.packet(packet);
  } else {
    this.sendBuffer.push(packet);
  }

  return this;
};

/**
 * Sends a packet.
 *
 * @param {Object} packet
 * @api private
 */

Socket.prototype.packet = function(packet){
  packet.nsp = this.nsp;
  this.io.packet(packet);
};

/**
 * Called upon engine `open`.
 *
 * @api private
 */

Socket.prototype.onopen = function(){
  debug('transport is open - connecting');

  // write connect packet if necessary
  if ('/' != this.nsp) {
    this.packet({ type: parser.CONNECT });
  }
};

/**
 * Called upon engine `close`.
 *
 * @param {String} reason
 * @api private
 */

Socket.prototype.onclose = function(reason){
  debug('close (%s)', reason);
  this.connected = false;
  this.disconnected = true;
  delete this.id;
  this.emit('disconnect', reason);
};

/**
 * Called with socket packet.
 *
 * @param {Object} packet
 * @api private
 */

Socket.prototype.onpacket = function(packet){
  if (packet.nsp != this.nsp) return;

  switch (packet.type) {
    case parser.CONNECT:
      this.onconnect();
      break;

    case parser.EVENT:
      this.onevent(packet);
      break;

    case parser.BINARY_EVENT:
      this.onevent(packet);
      break;

    case parser.ACK:
      this.onack(packet);
      break;

    case parser.BINARY_ACK:
      this.onack(packet);
      break;

    case parser.DISCONNECT:
      this.ondisconnect();
      break;

    case parser.ERROR:
      this.emit('error', packet.data);
      break;
  }
};

/**
 * Called upon a server event.
 *
 * @param {Object} packet
 * @api private
 */

Socket.prototype.onevent = function(packet){
  var args = packet.data || [];
  debug('emitting event %j', args);

  if (null != packet.id) {
    debug('attaching ack callback to event');
    args.push(this.ack(packet.id));
  }

  if (this.connected) {
    emit.apply(this, args);
  } else {
    this.receiveBuffer.push(args);
  }
};

/**
 * Produces an ack callback to emit with an event.
 *
 * @api private
 */

Socket.prototype.ack = function(id){
  var self = this;
  var sent = false;
  return function(){
    // prevent double callbacks
    if (sent) return;
    sent = true;
    var args = toArray(arguments);
    debug('sending ack %j', args);

    var type = hasBin(args) ? parser.BINARY_ACK : parser.ACK;
    self.packet({
      type: type,
      id: id,
      data: args
    });
  };
};

/**
 * Called upon a server acknowlegement.
 *
 * @param {Object} packet
 * @api private
 */

Socket.prototype.onack = function(packet){
  debug('calling ack %s with %j', packet.id, packet.data);
  var fn = this.acks[packet.id];
  fn.apply(this, packet.data);
  delete this.acks[packet.id];
};

/**
 * Called upon server connect.
 *
 * @api private
 */

Socket.prototype.onconnect = function(){
  this.connected = true;
  this.disconnected = false;
  this.emit('connect');
  this.emitBuffered();
};

/**
 * Emit buffered events (received and emitted).
 *
 * @api private
 */

Socket.prototype.emitBuffered = function(){
  var i;
  for (i = 0; i < this.receiveBuffer.length; i++) {
    emit.apply(this, this.receiveBuffer[i]);
  }
  this.receiveBuffer = [];

  for (i = 0; i < this.sendBuffer.length; i++) {
    this.packet(this.sendBuffer[i]);
  }
  this.sendBuffer = [];
};

/**
 * Called upon server disconnect.
 *
 * @api private
 */

Socket.prototype.ondisconnect = function(){
  debug('server disconnect (%s)', this.nsp);
  this.destroy();
  this.onclose('io server disconnect');
};

/**
 * Called upon forced client/server side disconnections,
 * this method ensures the manager stops tracking us and
 * that reconnections don't get triggered for this.
 *
 * @api private.
 */

Socket.prototype.destroy = function(){
  if (this.subs) {
    // clean subscriptions to avoid reconnections
    for (var i = 0; i < this.subs.length; i++) {
      this.subs[i].destroy();
    }
    this.subs = null;
  }

  this.io.destroy(this);
};

/**
 * Disconnects the socket manually.
 *
 * @return {Socket} self
 * @api public
 */

Socket.prototype.close =
Socket.prototype.disconnect = function(){
  if (this.connected) {
    debug('performing disconnect (%s)', this.nsp);
    this.packet({ type: parser.DISCONNECT });
  }

  // remove socket from pool
  this.destroy();

  if (this.connected) {
    // fire events
    this.onclose('io client disconnect');
  }
  return this;
};

},{"./on":4,"component-bind":8,"component-emitter":9,"debug":10,"has-binary":38,"socket.io-parser":46,"to-array":50}],6:[function(_dereq_,module,exports){
(function (global){

/**
 * Module dependencies.
 */

var parseuri = _dereq_('parseuri');
var debug = _dereq_('debug')('socket.io-client:url');

/**
 * Module exports.
 */

module.exports = url;

/**
 * URL parser.
 *
 * @param {String} url
 * @param {Object} An object meant to mimic window.location.
 *                 Defaults to window.location.
 * @api public
 */

function url(uri, loc){
  var obj = uri;

  // default to window.location
  var loc = loc || global.location;
  if (null == uri) uri = loc.protocol + '//' + loc.host;

  // relative path support
  if ('string' == typeof uri) {
    if ('/' == uri.charAt(0)) {
      if ('/' == uri.charAt(1)) {
        uri = loc.protocol + uri;
      } else {
        uri = loc.hostname + uri;
      }
    }

    if (!/^(https?|wss?):\/\//.test(uri)) {
      debug('protocol-less url %s', uri);
      if ('undefined' != typeof loc) {
        uri = loc.protocol + '//' + uri;
      } else {
        uri = 'https://' + uri;
      }
    }

    // parse
    debug('parse %s', uri);
    obj = parseuri(uri);
  }

  // make sure we treat `localhost:80` and `localhost` equally
  if (!obj.port) {
    if (/^(http|ws)$/.test(obj.protocol)) {
      obj.port = '80';
    }
    else if (/^(http|ws)s$/.test(obj.protocol)) {
      obj.port = '443';
    }
  }

  obj.path = obj.path || '/';

  // define unique id
  obj.id = obj.protocol + '://' + obj.host + ':' + obj.port;
  // define href
  obj.href = obj.protocol + '://' + obj.host + (loc && loc.port == obj.port ? '' : (':' + obj.port));

  return obj;
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"debug":10,"parseuri":44}],7:[function(_dereq_,module,exports){

/**
 * Expose `Backoff`.
 */

module.exports = Backoff;

/**
 * Initialize backoff timer with `opts`.
 *
 * - `min` initial timeout in milliseconds [100]
 * - `max` max timeout [10000]
 * - `jitter` [0]
 * - `factor` [2]
 *
 * @param {Object} opts
 * @api public
 */

function Backoff(opts) {
  opts = opts || {};
  this.ms = opts.min || 100;
  this.max = opts.max || 10000;
  this.factor = opts.factor || 2;
  this.jitter = opts.jitter > 0 && opts.jitter <= 1 ? opts.jitter : 0;
  this.attempts = 0;
}

/**
 * Return the backoff duration.
 *
 * @return {Number}
 * @api public
 */

Backoff.prototype.duration = function(){
  var ms = this.ms * Math.pow(this.factor, this.attempts++);
  if (this.jitter) {
    var rand =  Math.random();
    var deviation = Math.floor(rand * this.jitter * ms);
    ms = (Math.floor(rand * 10) & 1) == 0  ? ms - deviation : ms + deviation;
  }
  return Math.min(ms, this.max) | 0;
};

/**
 * Reset the number of attempts.
 *
 * @api public
 */

Backoff.prototype.reset = function(){
  this.attempts = 0;
};

/**
 * Set the minimum duration
 *
 * @api public
 */

Backoff.prototype.setMin = function(min){
  this.ms = min;
};

/**
 * Set the maximum duration
 *
 * @api public
 */

Backoff.prototype.setMax = function(max){
  this.max = max;
};

/**
 * Set the jitter
 *
 * @api public
 */

Backoff.prototype.setJitter = function(jitter){
  this.jitter = jitter;
};


},{}],8:[function(_dereq_,module,exports){
/**
 * Slice reference.
 */

var slice = [].slice;

/**
 * Bind `obj` to `fn`.
 *
 * @param {Object} obj
 * @param {Function|String} fn or string
 * @return {Function}
 * @api public
 */

module.exports = function(obj, fn){
  if ('string' == typeof fn) fn = obj[fn];
  if ('function' != typeof fn) throw new Error('bind() requires a function');
  var args = slice.call(arguments, 2);
  return function(){
    return fn.apply(obj, args.concat(slice.call(arguments)));
  }
};

},{}],9:[function(_dereq_,module,exports){

/**
 * Expose `Emitter`.
 */

module.exports = Emitter;

/**
 * Initialize a new `Emitter`.
 *
 * @api public
 */

function Emitter(obj) {
  if (obj) return mixin(obj);
};

/**
 * Mixin the emitter properties.
 *
 * @param {Object} obj
 * @return {Object}
 * @api private
 */

function mixin(obj) {
  for (var key in Emitter.prototype) {
    obj[key] = Emitter.prototype[key];
  }
  return obj;
}

/**
 * Listen on the given `event` with `fn`.
 *
 * @param {String} event
 * @param {Function} fn
 * @return {Emitter}
 * @api public
 */

Emitter.prototype.on =
Emitter.prototype.addEventListener = function(event, fn){
  this._callbacks = this._callbacks || {};
  (this._callbacks[event] = this._callbacks[event] || [])
    .push(fn);
  return this;
};

/**
 * Adds an `event` listener that will be invoked a single
 * time then automatically removed.
 *
 * @param {String} event
 * @param {Function} fn
 * @return {Emitter}
 * @api public
 */

Emitter.prototype.once = function(event, fn){
  var self = this;
  this._callbacks = this._callbacks || {};

  function on() {
    self.off(event, on);
    fn.apply(this, arguments);
  }

  on.fn = fn;
  this.on(event, on);
  return this;
};

/**
 * Remove the given callback for `event` or all
 * registered callbacks.
 *
 * @param {String} event
 * @param {Function} fn
 * @return {Emitter}
 * @api public
 */

Emitter.prototype.off =
Emitter.prototype.removeListener =
Emitter.prototype.removeAllListeners =
Emitter.prototype.removeEventListener = function(event, fn){
  this._callbacks = this._callbacks || {};

  // all
  if (0 == arguments.length) {
    this._callbacks = {};
    return this;
  }

  // specific event
  var callbacks = this._callbacks[event];
  if (!callbacks) return this;

  // remove all handlers
  if (1 == arguments.length) {
    delete this._callbacks[event];
    return this;
  }

  // remove specific handler
  var cb;
  for (var i = 0; i < callbacks.length; i++) {
    cb = callbacks[i];
    if (cb === fn || cb.fn === fn) {
      callbacks.splice(i, 1);
      break;
    }
  }
  return this;
};

/**
 * Emit `event` with the given args.
 *
 * @param {String} event
 * @param {Mixed} ...
 * @return {Emitter}
 */

Emitter.prototype.emit = function(event){
  this._callbacks = this._callbacks || {};
  var args = [].slice.call(arguments, 1)
    , callbacks = this._callbacks[event];

  if (callbacks) {
    callbacks = callbacks.slice(0);
    for (var i = 0, len = callbacks.length; i < len; ++i) {
      callbacks[i].apply(this, args);
    }
  }

  return this;
};

/**
 * Return array of callbacks for `event`.
 *
 * @param {String} event
 * @return {Array}
 * @api public
 */

Emitter.prototype.listeners = function(event){
  this._callbacks = this._callbacks || {};
  return this._callbacks[event] || [];
};

/**
 * Check if this emitter has `event` handlers.
 *
 * @param {String} event
 * @return {Boolean}
 * @api public
 */

Emitter.prototype.hasListeners = function(event){
  return !! this.listeners(event).length;
};

},{}],10:[function(_dereq_,module,exports){

/**
 * Expose `debug()` as the module.
 */

module.exports = debug;

/**
 * Create a debugger with the given `name`.
 *
 * @param {String} name
 * @return {Type}
 * @api public
 */

function debug(name) {
  if (!debug.enabled(name)) return function(){};

  return function(fmt){
    fmt = coerce(fmt);

    var curr = new Date;
    var ms = curr - (debug[name] || curr);
    debug[name] = curr;

    fmt = name
      + ' '
      + fmt
      + ' +' + debug.humanize(ms);

    // This hackery is required for IE8
    // where `console.log` doesn't have 'apply'
    window.console
      && console.log
      && Function.prototype.apply.call(console.log, console, arguments);
  }
}

/**
 * The currently active debug mode names.
 */

debug.names = [];
debug.skips = [];

/**
 * Enables a debug mode by name. This can include modes
 * separated by a colon and wildcards.
 *
 * @param {String} name
 * @api public
 */

debug.enable = function(name) {
  try {
    localStorage.debug = name;
  } catch(e){}

  var split = (name || '').split(/[\s,]+/)
    , len = split.length;

  for (var i = 0; i < len; i++) {
    name = split[i].replace('*', '.*?');
    if (name[0] === '-') {
      debug.skips.push(new RegExp('^' + name.substr(1) + '$'));
    }
    else {
      debug.names.push(new RegExp('^' + name + '$'));
    }
  }
};

/**
 * Disable debug output.
 *
 * @api public
 */

debug.disable = function(){
  debug.enable('');
};

/**
 * Humanize the given `ms`.
 *
 * @param {Number} m
 * @return {String}
 * @api private
 */

debug.humanize = function(ms) {
  var sec = 1000
    , min = 60 * 1000
    , hour = 60 * min;

  if (ms >= hour) return (ms / hour).toFixed(1) + 'h';
  if (ms >= min) return (ms / min).toFixed(1) + 'm';
  if (ms >= sec) return (ms / sec | 0) + 's';
  return ms + 'ms';
};

/**
 * Returns true if the given mode name is enabled, false otherwise.
 *
 * @param {String} name
 * @return {Boolean}
 * @api public
 */

debug.enabled = function(name) {
  for (var i = 0, len = debug.skips.length; i < len; i++) {
    if (debug.skips[i].test(name)) {
      return false;
    }
  }
  for (var i = 0, len = debug.names.length; i < len; i++) {
    if (debug.names[i].test(name)) {
      return true;
    }
  }
  return false;
};

/**
 * Coerce `val`.
 */

function coerce(val) {
  if (val instanceof Error) return val.stack || val.message;
  return val;
}

// persist

try {
  if (window.localStorage) debug.enable(localStorage.debug);
} catch(e){}

},{}],11:[function(_dereq_,module,exports){

module.exports =  _dereq_('./lib/');

},{"./lib/":12}],12:[function(_dereq_,module,exports){

module.exports = _dereq_('./socket');

/**
 * Exports parser
 *
 * @api public
 *
 */
module.exports.parser = _dereq_('engine.io-parser');

},{"./socket":13,"engine.io-parser":25}],13:[function(_dereq_,module,exports){
(function (global){
/**
 * Module dependencies.
 */

var transports = _dereq_('./transports');
var Emitter = _dereq_('component-emitter');
var debug = _dereq_('debug')('engine.io-client:socket');
var index = _dereq_('indexof');
var parser = _dereq_('engine.io-parser');
var parseuri = _dereq_('parseuri');
var parsejson = _dereq_('parsejson');
var parseqs = _dereq_('parseqs');

/**
 * Module exports.
 */

module.exports = Socket;

/**
 * Noop function.
 *
 * @api private
 */

function noop(){}

/**
 * Socket constructor.
 *
 * @param {String|Object} uri or options
 * @param {Object} options
 * @api public
 */

function Socket(uri, opts){
  if (!(this instanceof Socket)) return new Socket(uri, opts);

  opts = opts || {};

  if (uri && 'object' == typeof uri) {
    opts = uri;
    uri = null;
  }

  if (uri) {
    uri = parseuri(uri);
    opts.host = uri.host;
    opts.secure = uri.protocol == 'https' || uri.protocol == 'wss';
    opts.port = uri.port;
    if (uri.query) opts.query = uri.query;
  }

  this.secure = null != opts.secure ? opts.secure :
    (global.location && 'https:' == location.protocol);

  if (opts.host) {
    var pieces = opts.host.split(':');
    opts.hostname = pieces.shift();
    if (pieces.length) {
      opts.port = pieces.pop();
    } else if (!opts.port) {
      // if no port is specified manually, use the protocol default
      opts.port = this.secure ? '443' : '80';
    }
  }

  this.agent = opts.agent || false;
  this.hostname = opts.hostname ||
    (global.location ? location.hostname : 'localhost');
  this.port = opts.port || (global.location && location.port ?
       location.port :
       (this.secure ? 443 : 80));
  this.query = opts.query || {};
  if ('string' == typeof this.query) this.query = parseqs.decode(this.query);
  this.upgrade = false !== opts.upgrade;
  this.path = (opts.path || '/engine.io').replace(/\/$/, '') + '/';
  this.forceJSONP = !!opts.forceJSONP;
  this.jsonp = false !== opts.jsonp;
  this.forceBase64 = !!opts.forceBase64;
  this.enablesXDR = !!opts.enablesXDR;
  this.timestampParam = opts.timestampParam || 't';
  this.timestampRequests = opts.timestampRequests;
  this.transports = opts.transports || ['polling', 'websocket'];
  this.readyState = '';
  this.writeBuffer = [];
  this.callbackBuffer = [];
  this.policyPort = opts.policyPort || 843;
  this.rememberUpgrade = opts.rememberUpgrade || false;
  this.binaryType = null;
  this.onlyBinaryUpgrades = opts.onlyBinaryUpgrades;

  // SSL options for Node.js client
  this.pfx = opts.pfx || null;
  this.key = opts.key || null;
  this.passphrase = opts.passphrase || null;
  this.cert = opts.cert || null;
  this.ca = opts.ca || null;
  this.ciphers = opts.ciphers || null;
  this.rejectUnauthorized = opts.rejectUnauthorized || null;

  this.open();
}

Socket.priorWebsocketSuccess = false;

/**
 * Mix in `Emitter`.
 */

Emitter(Socket.prototype);

/**
 * Protocol version.
 *
 * @api public
 */

Socket.protocol = parser.protocol; // this is an int

/**
 * Expose deps for legacy compatibility
 * and standalone browser access.
 */

Socket.Socket = Socket;
Socket.Transport = _dereq_('./transport');
Socket.transports = _dereq_('./transports');
Socket.parser = _dereq_('engine.io-parser');

/**
 * Creates transport of the given type.
 *
 * @param {String} transport name
 * @return {Transport}
 * @api private
 */

Socket.prototype.createTransport = function (name) {
  debug('creating transport "%s"', name);
  var query = clone(this.query);

  // append engine.io protocol identifier
  query.EIO = parser.protocol;

  // transport name
  query.transport = name;

  // session id if we already have one
  if (this.id) query.sid = this.id;

  var transport = new transports[name]({
    agent: this.agent,
    hostname: this.hostname,
    port: this.port,
    secure: this.secure,
    path: this.path,
    query: query,
    forceJSONP: this.forceJSONP,
    jsonp: this.jsonp,
    forceBase64: this.forceBase64,
    enablesXDR: this.enablesXDR,
    timestampRequests: this.timestampRequests,
    timestampParam: this.timestampParam,
    policyPort: this.policyPort,
    socket: this,
    pfx: this.pfx,
    key: this.key,
    passphrase: this.passphrase,
    cert: this.cert,
    ca: this.ca,
    ciphers: this.ciphers,
    rejectUnauthorized: this.rejectUnauthorized
  });

  return transport;
};

function clone (obj) {
  var o = {};
  for (var i in obj) {
    if (obj.hasOwnProperty(i)) {
      o[i] = obj[i];
    }
  }
  return o;
}

/**
 * Initializes transport to use and starts probe.
 *
 * @api private
 */
Socket.prototype.open = function () {
  var transport;
  if (this.rememberUpgrade && Socket.priorWebsocketSuccess && this.transports.indexOf('websocket') != -1) {
    transport = 'websocket';
  } else if (0 == this.transports.length) {
    // Emit error on next tick so it can be listened to
    var self = this;
    setTimeout(function() {
      self.emit('error', 'No transports available');
    }, 0);
    return;
  } else {
    transport = this.transports[0];
  }
  this.readyState = 'opening';

  // Retry with the next transport if the transport is disabled (jsonp: false)
  var transport;
  try {
    transport = this.createTransport(transport);
  } catch (e) {
    this.transports.shift();
    this.open();
    return;
  }

  transport.open();
  this.setTransport(transport);
};

/**
 * Sets the current transport. Disables the existing one (if any).
 *
 * @api private
 */

Socket.prototype.setTransport = function(transport){
  debug('setting transport %s', transport.name);
  var self = this;

  if (this.transport) {
    debug('clearing existing transport %s', this.transport.name);
    this.transport.removeAllListeners();
  }

  // set up transport
  this.transport = transport;

  // set up transport listeners
  transport
  .on('drain', function(){
    self.onDrain();
  })
  .on('packet', function(packet){
    self.onPacket(packet);
  })
  .on('error', function(e){
    self.onError(e);
  })
  .on('close', function(){
    self.onClose('transport close');
  });
};

/**
 * Probes a transport.
 *
 * @param {String} transport name
 * @api private
 */

Socket.prototype.probe = function (name) {
  debug('probing transport "%s"', name);
  var transport = this.createTransport(name, { probe: 1 })
    , failed = false
    , self = this;

  Socket.priorWebsocketSuccess = false;

  function onTransportOpen(){
    if (self.onlyBinaryUpgrades) {
      var upgradeLosesBinary = !this.supportsBinary && self.transport.supportsBinary;
      failed = failed || upgradeLosesBinary;
    }
    if (failed) return;

    debug('probe transport "%s" opened', name);
    transport.send([{ type: 'ping', data: 'probe' }]);
    transport.once('packet', function (msg) {
      if (failed) return;
      if ('pong' == msg.type && 'probe' == msg.data) {
        debug('probe transport "%s" pong', name);
        self.upgrading = true;
        self.emit('upgrading', transport);
        if (!transport) return;
        Socket.priorWebsocketSuccess = 'websocket' == transport.name;

        debug('pausing current transport "%s"', self.transport.name);
        self.transport.pause(function () {
          if (failed) return;
          if ('closed' == self.readyState) return;
          debug('changing transport and sending upgrade packet');

          cleanup();

          self.setTransport(transport);
          transport.send([{ type: 'upgrade' }]);
          self.emit('upgrade', transport);
          transport = null;
          self.upgrading = false;
          self.flush();
        });
      } else {
        debug('probe transport "%s" failed', name);
        var err = new Error('probe error');
        err.transport = transport.name;
        self.emit('upgradeError', err);
      }
    });
  }

  function freezeTransport() {
    if (failed) return;

    // Any callback called by transport should be ignored since now
    failed = true;

    cleanup();

    transport.close();
    transport = null;
  }

  //Handle any error that happens while probing
  function onerror(err) {
    var error = new Error('probe error: ' + err);
    error.transport = transport.name;

    freezeTransport();

    debug('probe transport "%s" failed because of error: %s', name, err);

    self.emit('upgradeError', error);
  }

  function onTransportClose(){
    onerror("transport closed");
  }

  //When the socket is closed while we're probing
  function onclose(){
    onerror("socket closed");
  }

  //When the socket is upgraded while we're probing
  function onupgrade(to){
    if (transport && to.name != transport.name) {
      debug('"%s" works - aborting "%s"', to.name, transport.name);
      freezeTransport();
    }
  }

  //Remove all listeners on the transport and on self
  function cleanup(){
    transport.removeListener('open', onTransportOpen);
    transport.removeListener('error', onerror);
    transport.removeListener('close', onTransportClose);
    self.removeListener('close', onclose);
    self.removeListener('upgrading', onupgrade);
  }

  transport.once('open', onTransportOpen);
  transport.once('error', onerror);
  transport.once('close', onTransportClose);

  this.once('close', onclose);
  this.once('upgrading', onupgrade);

  transport.open();

};

/**
 * Called when connection is deemed open.
 *
 * @api public
 */

Socket.prototype.onOpen = function () {
  debug('socket open');
  this.readyState = 'open';
  Socket.priorWebsocketSuccess = 'websocket' == this.transport.name;
  this.emit('open');
  this.flush();

  // we check for `readyState` in case an `open`
  // listener already closed the socket
  if ('open' == this.readyState && this.upgrade && this.transport.pause) {
    debug('starting upgrade probes');
    for (var i = 0, l = this.upgrades.length; i < l; i++) {
      this.probe(this.upgrades[i]);
    }
  }
};

/**
 * Handles a packet.
 *
 * @api private
 */

Socket.prototype.onPacket = function (packet) {
  if ('opening' == this.readyState || 'open' == this.readyState) {
    debug('socket receive: type "%s", data "%s"', packet.type, packet.data);

    this.emit('packet', packet);

    // Socket is live - any packet counts
    this.emit('heartbeat');

    switch (packet.type) {
      case 'open':
        this.onHandshake(parsejson(packet.data));
        break;

      case 'pong':
        this.setPing();
        break;

      case 'error':
        var err = new Error('server error');
        err.code = packet.data;
        this.emit('error', err);
        break;

      case 'message':
        this.emit('data', packet.data);
        this.emit('message', packet.data);
        break;
    }
  } else {
    debug('packet received with socket readyState "%s"', this.readyState);
  }
};

/**
 * Called upon handshake completion.
 *
 * @param {Object} handshake obj
 * @api private
 */

Socket.prototype.onHandshake = function (data) {
  this.emit('handshake', data);
  this.id = data.sid;
  this.transport.query.sid = data.sid;
  this.upgrades = this.filterUpgrades(data.upgrades);
  this.pingInterval = data.pingInterval;
  this.pingTimeout = data.pingTimeout;
  this.onOpen();
  // In case open handler closes socket
  if  ('closed' == this.readyState) return;
  this.setPing();

  // Prolong liveness of socket on heartbeat
  this.removeListener('heartbeat', this.onHeartbeat);
  this.on('heartbeat', this.onHeartbeat);
};

/**
 * Resets ping timeout.
 *
 * @api private
 */

Socket.prototype.onHeartbeat = function (timeout) {
  clearTimeout(this.pingTimeoutTimer);
  var self = this;
  self.pingTimeoutTimer = setTimeout(function () {
    if ('closed' == self.readyState) return;
    self.onClose('ping timeout');
  }, timeout || (self.pingInterval + self.pingTimeout));
};

/**
 * Pings server every `this.pingInterval` and expects response
 * within `this.pingTimeout` or closes connection.
 *
 * @api private
 */

Socket.prototype.setPing = function () {
  var self = this;
  clearTimeout(self.pingIntervalTimer);
  self.pingIntervalTimer = setTimeout(function () {
    debug('writing ping packet - expecting pong within %sms', self.pingTimeout);
    self.ping();
    self.onHeartbeat(self.pingTimeout);
  }, self.pingInterval);
};

/**
* Sends a ping packet.
*
* @api public
*/

Socket.prototype.ping = function () {
  this.sendPacket('ping');
};

/**
 * Called on `drain` event
 *
 * @api private
 */

Socket.prototype.onDrain = function() {
  for (var i = 0; i < this.prevBufferLen; i++) {
    if (this.callbackBuffer[i]) {
      this.callbackBuffer[i]();
    }
  }

  this.writeBuffer.splice(0, this.prevBufferLen);
  this.callbackBuffer.splice(0, this.prevBufferLen);

  // setting prevBufferLen = 0 is very important
  // for example, when upgrading, upgrade packet is sent over,
  // and a nonzero prevBufferLen could cause problems on `drain`
  this.prevBufferLen = 0;

  if (this.writeBuffer.length == 0) {
    this.emit('drain');
  } else {
    this.flush();
  }
};

/**
 * Flush write buffers.
 *
 * @api private
 */

Socket.prototype.flush = function () {
  if ('closed' != this.readyState && this.transport.writable &&
    !this.upgrading && this.writeBuffer.length) {
    debug('flushing %d packets in socket', this.writeBuffer.length);
    this.transport.send(this.writeBuffer);
    // keep track of current length of writeBuffer
    // splice writeBuffer and callbackBuffer on `drain`
    this.prevBufferLen = this.writeBuffer.length;
    this.emit('flush');
  }
};

/**
 * Sends a message.
 *
 * @param {String} message.
 * @param {Function} callback function.
 * @return {Socket} for chaining.
 * @api public
 */

Socket.prototype.write =
Socket.prototype.send = function (msg, fn) {
  this.sendPacket('message', msg, fn);
  return this;
};

/**
 * Sends a packet.
 *
 * @param {String} packet type.
 * @param {String} data.
 * @param {Function} callback function.
 * @api private
 */

Socket.prototype.sendPacket = function (type, data, fn) {
  if ('closing' == this.readyState || 'closed' == this.readyState) {
    return;
  }

  var packet = { type: type, data: data };
  this.emit('packetCreate', packet);
  this.writeBuffer.push(packet);
  this.callbackBuffer.push(fn);
  this.flush();
};

/**
 * Closes the connection.
 *
 * @api private
 */

Socket.prototype.close = function () {
  if ('opening' == this.readyState || 'open' == this.readyState) {
    this.readyState = 'closing';

    var self = this;

    function close() {
      self.onClose('forced close');
      debug('socket closing - telling transport to close');
      self.transport.close();
    }

    function cleanupAndClose() {
      self.removeListener('upgrade', cleanupAndClose);
      self.removeListener('upgradeError', cleanupAndClose);
      close();
    }

    function waitForUpgrade() {
      // wait for upgrade to finish since we can't send packets while pausing a transport
      self.once('upgrade', cleanupAndClose);
      self.once('upgradeError', cleanupAndClose);
    }

    if (this.writeBuffer.length) {
      this.once('drain', function() {
        if (this.upgrading) {
          waitForUpgrade();
        } else {
          close();
        }
      });
    } else if (this.upgrading) {
      waitForUpgrade();
    } else {
      close();
    }
  }

  return this;
};

/**
 * Called upon transport error
 *
 * @api private
 */

Socket.prototype.onError = function (err) {
  debug('socket error %j', err);
  Socket.priorWebsocketSuccess = false;
  this.emit('error', err);
  this.onClose('transport error', err);
};

/**
 * Called upon transport close.
 *
 * @api private
 */

Socket.prototype.onClose = function (reason, desc) {
  if ('opening' == this.readyState || 'open' == this.readyState || 'closing' == this.readyState) {
    debug('socket close with reason: "%s"', reason);
    var self = this;

    // clear timers
    clearTimeout(this.pingIntervalTimer);
    clearTimeout(this.pingTimeoutTimer);

    // clean buffers in next tick, so developers can still
    // grab the buffers on `close` event
    setTimeout(function() {
      self.writeBuffer = [];
      self.callbackBuffer = [];
      self.prevBufferLen = 0;
    }, 0);

    // stop event from firing again for transport
    this.transport.removeAllListeners('close');

    // ensure transport won't stay open
    this.transport.close();

    // ignore further transport communication
    this.transport.removeAllListeners();

    // set ready state
    this.readyState = 'closed';

    // clear session id
    this.id = null;

    // emit close event
    this.emit('close', reason, desc);
  }
};

/**
 * Filters upgrades, returning only those matching client transports.
 *
 * @param {Array} server upgrades
 * @api private
 *
 */

Socket.prototype.filterUpgrades = function (upgrades) {
  var filteredUpgrades = [];
  for (var i = 0, j = upgrades.length; i<j; i++) {
    if (~index(this.transports, upgrades[i])) filteredUpgrades.push(upgrades[i]);
  }
  return filteredUpgrades;
};

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./transport":14,"./transports":15,"component-emitter":9,"debug":22,"engine.io-parser":25,"indexof":42,"parsejson":34,"parseqs":35,"parseuri":36}],14:[function(_dereq_,module,exports){
/**
 * Module dependencies.
 */

var parser = _dereq_('engine.io-parser');
var Emitter = _dereq_('component-emitter');

/**
 * Module exports.
 */

module.exports = Transport;

/**
 * Transport abstract constructor.
 *
 * @param {Object} options.
 * @api private
 */

function Transport (opts) {
  this.path = opts.path;
  this.hostname = opts.hostname;
  this.port = opts.port;
  this.secure = opts.secure;
  this.query = opts.query;
  this.timestampParam = opts.timestampParam;
  this.timestampRequests = opts.timestampRequests;
  this.readyState = '';
  this.agent = opts.agent || false;
  this.socket = opts.socket;
  this.enablesXDR = opts.enablesXDR;

  // SSL options for Node.js client
  this.pfx = opts.pfx;
  this.key = opts.key;
  this.passphrase = opts.passphrase;
  this.cert = opts.cert;
  this.ca = opts.ca;
  this.ciphers = opts.ciphers;
  this.rejectUnauthorized = opts.rejectUnauthorized;
}

/**
 * Mix in `Emitter`.
 */

Emitter(Transport.prototype);

/**
 * A counter used to prevent collisions in the timestamps used
 * for cache busting.
 */

Transport.timestamps = 0;

/**
 * Emits an error.
 *
 * @param {String} str
 * @return {Transport} for chaining
 * @api public
 */

Transport.prototype.onError = function (msg, desc) {
  var err = new Error(msg);
  err.type = 'TransportError';
  err.description = desc;
  this.emit('error', err);
  return this;
};

/**
 * Opens the transport.
 *
 * @api public
 */

Transport.prototype.open = function () {
  if ('closed' == this.readyState || '' == this.readyState) {
    this.readyState = 'opening';
    this.doOpen();
  }

  return this;
};

/**
 * Closes the transport.
 *
 * @api private
 */

Transport.prototype.close = function () {
  if ('opening' == this.readyState || 'open' == this.readyState) {
    this.doClose();
    this.onClose();
  }

  return this;
};

/**
 * Sends multiple packets.
 *
 * @param {Array} packets
 * @api private
 */

Transport.prototype.send = function(packets){
  if ('open' == this.readyState) {
    this.write(packets);
  } else {
    throw new Error('Transport not open');
  }
};

/**
 * Called upon open
 *
 * @api private
 */

Transport.prototype.onOpen = function () {
  this.readyState = 'open';
  this.writable = true;
  this.emit('open');
};

/**
 * Called with data.
 *
 * @param {String} data
 * @api private
 */

Transport.prototype.onData = function(data){
  var packet = parser.decodePacket(data, this.socket.binaryType);
  this.onPacket(packet);
};

/**
 * Called with a decoded packet.
 */

Transport.prototype.onPacket = function (packet) {
  this.emit('packet', packet);
};

/**
 * Called upon close.
 *
 * @api private
 */

Transport.prototype.onClose = function () {
  this.readyState = 'closed';
  this.emit('close');
};

},{"component-emitter":9,"engine.io-parser":25}],15:[function(_dereq_,module,exports){
(function (global){
/**
 * Module dependencies
 */

var XMLHttpRequest = _dereq_('xmlhttprequest');
var XHR = _dereq_('./polling-xhr');
var JSONP = _dereq_('./polling-jsonp');
var websocket = _dereq_('./websocket');

/**
 * Export transports.
 */

exports.polling = polling;
exports.websocket = websocket;

/**
 * Polling transport polymorphic constructor.
 * Decides on xhr vs jsonp based on feature detection.
 *
 * @api private
 */

function polling(opts){
  var xhr;
  var xd = false;
  var xs = false;
  var jsonp = false !== opts.jsonp;

  if (global.location) {
    var isSSL = 'https:' == location.protocol;
    var port = location.port;

    // some user agents have empty `location.port`
    if (!port) {
      port = isSSL ? 443 : 80;
    }

    xd = opts.hostname != location.hostname || port != opts.port;
    xs = opts.secure != isSSL;
  }

  opts.xdomain = xd;
  opts.xscheme = xs;
  xhr = new XMLHttpRequest(opts);

  if ('open' in xhr && !opts.forceJSONP) {
    return new XHR(opts);
  } else {
    if (!jsonp) throw new Error('JSONP disabled');
    return new JSONP(opts);
  }
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./polling-jsonp":16,"./polling-xhr":17,"./websocket":19,"xmlhttprequest":20}],16:[function(_dereq_,module,exports){
(function (global){

/**
 * Module requirements.
 */

var Polling = _dereq_('./polling');
var inherit = _dereq_('component-inherit');

/**
 * Module exports.
 */

module.exports = JSONPPolling;

/**
 * Cached regular expressions.
 */

var rNewline = /\n/g;
var rEscapedNewline = /\\n/g;

/**
 * Global JSONP callbacks.
 */

var callbacks;

/**
 * Callbacks count.
 */

var index = 0;

/**
 * Noop.
 */

function empty () { }

/**
 * JSONP Polling constructor.
 *
 * @param {Object} opts.
 * @api public
 */

function JSONPPolling (opts) {
  Polling.call(this, opts);

  this.query = this.query || {};

  // define global callbacks array if not present
  // we do this here (lazily) to avoid unneeded global pollution
  if (!callbacks) {
    // we need to consider multiple engines in the same page
    if (!global.___eio) global.___eio = [];
    callbacks = global.___eio;
  }

  // callback identifier
  this.index = callbacks.length;

  // add callback to jsonp global
  var self = this;
  callbacks.push(function (msg) {
    self.onData(msg);
  });

  // append to query string
  this.query.j = this.index;

  // prevent spurious errors from being emitted when the window is unloaded
  if (global.document && global.addEventListener) {
    global.addEventListener('beforeunload', function () {
      if (self.script) self.script.onerror = empty;
    }, false);
  }
}

/**
 * Inherits from Polling.
 */

inherit(JSONPPolling, Polling);

/*
 * JSONP only supports binary as base64 encoded strings
 */

JSONPPolling.prototype.supportsBinary = false;

/**
 * Closes the socket.
 *
 * @api private
 */

JSONPPolling.prototype.doClose = function () {
  if (this.script) {
    this.script.parentNode.removeChild(this.script);
    this.script = null;
  }

  if (this.form) {
    this.form.parentNode.removeChild(this.form);
    this.form = null;
    this.iframe = null;
  }

  Polling.prototype.doClose.call(this);
};

/**
 * Starts a poll cycle.
 *
 * @api private
 */

JSONPPolling.prototype.doPoll = function () {
  var self = this;
  var script = document.createElement('script');

  if (this.script) {
    this.script.parentNode.removeChild(this.script);
    this.script = null;
  }

  script.async = true;
  script.src = this.uri();
  script.onerror = function(e){
    self.onError('jsonp poll error',e);
  };

  var insertAt = document.getElementsByTagName('script')[0];
  insertAt.parentNode.insertBefore(script, insertAt);
  this.script = script;

  var isUAgecko = 'undefined' != typeof navigator && /gecko/i.test(navigator.userAgent);

  if (isUAgecko) {
    setTimeout(function () {
      var iframe = document.createElement('iframe');
      document.body.appendChild(iframe);
      document.body.removeChild(iframe);
    }, 100);
  }
};

/**
 * Writes with a hidden iframe.
 *
 * @param {String} data to send
 * @param {Function} called upon flush.
 * @api private
 */

JSONPPolling.prototype.doWrite = function (data, fn) {
  var self = this;

  if (!this.form) {
    var form = document.createElement('form');
    var area = document.createElement('textarea');
    var id = this.iframeId = 'eio_iframe_' + this.index;
    var iframe;

    form.className = 'socketio';
    form.style.position = 'absolute';
    form.style.top = '-1000px';
    form.style.left = '-1000px';
    form.target = id;
    form.method = 'POST';
    form.setAttribute('accept-charset', 'utf-8');
    area.name = 'd';
    form.appendChild(area);
    document.body.appendChild(form);

    this.form = form;
    this.area = area;
  }

  this.form.action = this.uri();

  function complete () {
    initIframe();
    fn();
  }

  function initIframe () {
    if (self.iframe) {
      try {
        self.form.removeChild(self.iframe);
      } catch (e) {
        self.onError('jsonp polling iframe removal error', e);
      }
    }

    try {
      // ie6 dynamic iframes with target="" support (thanks Chris Lambacher)
      var html = '<iframe src="javascript:0" name="'+ self.iframeId +'">';
      iframe = document.createElement(html);
    } catch (e) {
      iframe = document.createElement('iframe');
      iframe.name = self.iframeId;
      iframe.src = 'javascript:0';
    }

    iframe.id = self.iframeId;

    self.form.appendChild(iframe);
    self.iframe = iframe;
  }

  initIframe();

  // escape \n to prevent it from being converted into \r\n by some UAs
  // double escaping is required for escaped new lines because unescaping of new lines can be done safely on server-side
  data = data.replace(rEscapedNewline, '\\\n');
  this.area.value = data.replace(rNewline, '\\n');

  try {
    this.form.submit();
  } catch(e) {}

  if (this.iframe.attachEvent) {
    this.iframe.onreadystatechange = function(){
      if (self.iframe.readyState == 'complete') {
        complete();
      }
    };
  } else {
    this.iframe.onload = complete;
  }
};

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./polling":18,"component-inherit":21}],17:[function(_dereq_,module,exports){
(function (global){
/**
 * Module requirements.
 */

var XMLHttpRequest = _dereq_('xmlhttprequest');
var Polling = _dereq_('./polling');
var Emitter = _dereq_('component-emitter');
var inherit = _dereq_('component-inherit');
var debug = _dereq_('debug')('engine.io-client:polling-xhr');

/**
 * Module exports.
 */

module.exports = XHR;
module.exports.Request = Request;

/**
 * Empty function
 */

function empty(){}

/**
 * XHR Polling constructor.
 *
 * @param {Object} opts
 * @api public
 */

function XHR(opts){
  Polling.call(this, opts);

  if (global.location) {
    var isSSL = 'https:' == location.protocol;
    var port = location.port;

    // some user agents have empty `location.port`
    if (!port) {
      port = isSSL ? 443 : 80;
    }

    this.xd = opts.hostname != global.location.hostname ||
      port != opts.port;
    this.xs = opts.secure != isSSL;
  }
}

/**
 * Inherits from Polling.
 */

inherit(XHR, Polling);

/**
 * XHR supports binary
 */

XHR.prototype.supportsBinary = true;

/**
 * Creates a request.
 *
 * @param {String} method
 * @api private
 */

XHR.prototype.request = function(opts){
  opts = opts || {};
  opts.uri = this.uri();
  opts.xd = this.xd;
  opts.xs = this.xs;
  opts.agent = this.agent || false;
  opts.supportsBinary = this.supportsBinary;
  opts.enablesXDR = this.enablesXDR;

  // SSL options for Node.js client
  opts.pfx = this.pfx;
  opts.key = this.key;
  opts.passphrase = this.passphrase;
  opts.cert = this.cert;
  opts.ca = this.ca;
  opts.ciphers = this.ciphers;
  opts.rejectUnauthorized = this.rejectUnauthorized;

  return new Request(opts);
};

/**
 * Sends data.
 *
 * @param {String} data to send.
 * @param {Function} called upon flush.
 * @api private
 */

XHR.prototype.doWrite = function(data, fn){
  var isBinary = typeof data !== 'string' && data !== undefined;
  var req = this.request({ method: 'POST', data: data, isBinary: isBinary });
  var self = this;
  req.on('success', fn);
  req.on('error', function(err){
    self.onError('xhr post error', err);
  });
  this.sendXhr = req;
};

/**
 * Starts a poll cycle.
 *
 * @api private
 */

XHR.prototype.doPoll = function(){
  debug('xhr poll');
  var req = this.request();
  var self = this;
  req.on('data', function(data){
    self.onData(data);
  });
  req.on('error', function(err){
    self.onError('xhr poll error', err);
  });
  this.pollXhr = req;
};

/**
 * Request constructor
 *
 * @param {Object} options
 * @api public
 */

function Request(opts){
  this.method = opts.method || 'GET';
  this.uri = opts.uri;
  this.xd = !!opts.xd;
  this.xs = !!opts.xs;
  this.async = false !== opts.async;
  this.data = undefined != opts.data ? opts.data : null;
  this.agent = opts.agent;
  this.isBinary = opts.isBinary;
  this.supportsBinary = opts.supportsBinary;
  this.enablesXDR = opts.enablesXDR;

  // SSL options for Node.js client
  this.pfx = opts.pfx;
  this.key = opts.key;
  this.passphrase = opts.passphrase;
  this.cert = opts.cert;
  this.ca = opts.ca;
  this.ciphers = opts.ciphers;
  this.rejectUnauthorized = opts.rejectUnauthorized;

  this.create();
}

/**
 * Mix in `Emitter`.
 */

Emitter(Request.prototype);

/**
 * Creates the XHR object and sends the request.
 *
 * @api private
 */

Request.prototype.create = function(){
  var opts = { agent: this.agent, xdomain: this.xd, xscheme: this.xs, enablesXDR: this.enablesXDR };

  // SSL options for Node.js client
  opts.pfx = this.pfx;
  opts.key = this.key;
  opts.passphrase = this.passphrase;
  opts.cert = this.cert;
  opts.ca = this.ca;
  opts.ciphers = this.ciphers;
  opts.rejectUnauthorized = this.rejectUnauthorized;

  var xhr = this.xhr = new XMLHttpRequest(opts);
  var self = this;

  try {
    debug('xhr open %s: %s', this.method, this.uri);
    xhr.open(this.method, this.uri, this.async);
    if (this.supportsBinary) {
      // This has to be done after open because Firefox is stupid
      // http://stackoverflow.com/questions/13216903/get-binary-data-with-xmlhttprequest-in-a-firefox-extension
      xhr.responseType = 'arraybuffer';
    }

    if ('POST' == this.method) {
      try {
        if (this.isBinary) {
          xhr.setRequestHeader('Content-type', 'application/octet-stream');
        } else {
          xhr.setRequestHeader('Content-type', 'text/plain;charset=UTF-8');
        }
      } catch (e) {}
    }

    // ie6 check
    if ('withCredentials' in xhr) {
      xhr.withCredentials = true;
    }

    if (this.hasXDR()) {
      xhr.onload = function(){
        self.onLoad();
      };
      xhr.onerror = function(){
        self.onError(xhr.responseText);
      };
    } else {
      xhr.onreadystatechange = function(){
        if (4 != xhr.readyState) return;
        if (200 == xhr.status || 1223 == xhr.status) {
          self.onLoad();
        } else {
          // make sure the `error` event handler that's user-set
          // does not throw in the same tick and gets caught here
          setTimeout(function(){
            self.onError(xhr.status);
          }, 0);
        }
      };
    }

    debug('xhr data %s', this.data);
    xhr.send(this.data);
  } catch (e) {
    // Need to defer since .create() is called directly fhrom the constructor
    // and thus the 'error' event can only be only bound *after* this exception
    // occurs.  Therefore, also, we cannot throw here at all.
    setTimeout(function() {
      self.onError(e);
    }, 0);
    return;
  }

  if (global.document) {
    this.index = Request.requestsCount++;
    Request.requests[this.index] = this;
  }
};

/**
 * Called upon successful response.
 *
 * @api private
 */

Request.prototype.onSuccess = function(){
  this.emit('success');
  this.cleanup();
};

/**
 * Called if we have data.
 *
 * @api private
 */

Request.prototype.onData = function(data){
  this.emit('data', data);
  this.onSuccess();
};

/**
 * Called upon error.
 *
 * @api private
 */

Request.prototype.onError = function(err){
  this.emit('error', err);
  this.cleanup(true);
};

/**
 * Cleans up house.
 *
 * @api private
 */

Request.prototype.cleanup = function(fromError){
  if ('undefined' == typeof this.xhr || null === this.xhr) {
    return;
  }
  // xmlhttprequest
  if (this.hasXDR()) {
    this.xhr.onload = this.xhr.onerror = empty;
  } else {
    this.xhr.onreadystatechange = empty;
  }

  if (fromError) {
    try {
      this.xhr.abort();
    } catch(e) {}
  }

  if (global.document) {
    delete Request.requests[this.index];
  }

  this.xhr = null;
};

/**
 * Called upon load.
 *
 * @api private
 */

Request.prototype.onLoad = function(){
  var data;
  try {
    var contentType;
    try {
      contentType = this.xhr.getResponseHeader('Content-Type').split(';')[0];
    } catch (e) {}
    if (contentType === 'application/octet-stream') {
      data = this.xhr.response;
    } else {
      if (!this.supportsBinary) {
        data = this.xhr.responseText;
      } else {
        data = 'ok';
      }
    }
  } catch (e) {
    this.onError(e);
  }
  if (null != data) {
    this.onData(data);
  }
};

/**
 * Check if it has XDomainRequest.
 *
 * @api private
 */

Request.prototype.hasXDR = function(){
  return 'undefined' !== typeof global.XDomainRequest && !this.xs && this.enablesXDR;
};

/**
 * Aborts the request.
 *
 * @api public
 */

Request.prototype.abort = function(){
  this.cleanup();
};

/**
 * Aborts pending requests when unloading the window. This is needed to prevent
 * memory leaks (e.g. when using IE) and to ensure that no spurious error is
 * emitted.
 */

if (global.document) {
  Request.requestsCount = 0;
  Request.requests = {};
  if (global.attachEvent) {
    global.attachEvent('onunload', unloadHandler);
  } else if (global.addEventListener) {
    global.addEventListener('beforeunload', unloadHandler, false);
  }
}

function unloadHandler() {
  for (var i in Request.requests) {
    if (Request.requests.hasOwnProperty(i)) {
      Request.requests[i].abort();
    }
  }
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./polling":18,"component-emitter":9,"component-inherit":21,"debug":22,"xmlhttprequest":20}],18:[function(_dereq_,module,exports){
/**
 * Module dependencies.
 */

var Transport = _dereq_('../transport');
var parseqs = _dereq_('parseqs');
var parser = _dereq_('engine.io-parser');
var inherit = _dereq_('component-inherit');
var debug = _dereq_('debug')('engine.io-client:polling');

/**
 * Module exports.
 */

module.exports = Polling;

/**
 * Is XHR2 supported?
 */

var hasXHR2 = (function() {
  var XMLHttpRequest = _dereq_('xmlhttprequest');
  var xhr = new XMLHttpRequest({ xdomain: false });
  return null != xhr.responseType;
})();

/**
 * Polling interface.
 *
 * @param {Object} opts
 * @api private
 */

function Polling(opts){
  var forceBase64 = (opts && opts.forceBase64);
  if (!hasXHR2 || forceBase64) {
    this.supportsBinary = false;
  }
  Transport.call(this, opts);
}

/**
 * Inherits from Transport.
 */

inherit(Polling, Transport);

/**
 * Transport name.
 */

Polling.prototype.name = 'polling';

/**
 * Opens the socket (triggers polling). We write a PING message to determine
 * when the transport is open.
 *
 * @api private
 */

Polling.prototype.doOpen = function(){
  this.poll();
};

/**
 * Pauses polling.
 *
 * @param {Function} callback upon buffers are flushed and transport is paused
 * @api private
 */

Polling.prototype.pause = function(onPause){
  var pending = 0;
  var self = this;

  this.readyState = 'pausing';

  function pause(){
    debug('paused');
    self.readyState = 'paused';
    onPause();
  }

  if (this.polling || !this.writable) {
    var total = 0;

    if (this.polling) {
      debug('we are currently polling - waiting to pause');
      total++;
      this.once('pollComplete', function(){
        debug('pre-pause polling complete');
        --total || pause();
      });
    }

    if (!this.writable) {
      debug('we are currently writing - waiting to pause');
      total++;
      this.once('drain', function(){
        debug('pre-pause writing complete');
        --total || pause();
      });
    }
  } else {
    pause();
  }
};

/**
 * Starts polling cycle.
 *
 * @api public
 */

Polling.prototype.poll = function(){
  debug('polling');
  this.polling = true;
  this.doPoll();
  this.emit('poll');
};

/**
 * Overloads onData to detect payloads.
 *
 * @api private
 */

Polling.prototype.onData = function(data){
  var self = this;
  debug('polling got data %s', data);
  var callback = function(packet, index, total) {
    // if its the first message we consider the transport open
    if ('opening' == self.readyState) {
      self.onOpen();
    }

    // if its a close packet, we close the ongoing requests
    if ('close' == packet.type) {
      self.onClose();
      return false;
    }

    // otherwise bypass onData and handle the message
    self.onPacket(packet);
  };

  // decode payload
  parser.decodePayload(data, this.socket.binaryType, callback);

  // if an event did not trigger closing
  if ('closed' != this.readyState) {
    // if we got data we're not polling
    this.polling = false;
    this.emit('pollComplete');

    if ('open' == this.readyState) {
      this.poll();
    } else {
      debug('ignoring poll - transport state "%s"', this.readyState);
    }
  }
};

/**
 * For polling, send a close packet.
 *
 * @api private
 */

Polling.prototype.doClose = function(){
  var self = this;

  function close(){
    debug('writing close packet');
    self.write([{ type: 'close' }]);
  }

  if ('open' == this.readyState) {
    debug('transport open - closing');
    close();
  } else {
    // in case we're trying to close while
    // handshaking is in progress (GH-164)
    debug('transport not open - deferring close');
    this.once('open', close);
  }
};

/**
 * Writes a packets payload.
 *
 * @param {Array} data packets
 * @param {Function} drain callback
 * @api private
 */

Polling.prototype.write = function(packets){
  var self = this;
  this.writable = false;
  var callbackfn = function() {
    self.writable = true;
    self.emit('drain');
  };

  var self = this;
  parser.encodePayload(packets, this.supportsBinary, function(data) {
    self.doWrite(data, callbackfn);
  });
};

/**
 * Generates uri for connection.
 *
 * @api private
 */

Polling.prototype.uri = function(){
  var query = this.query || {};
  var schema = this.secure ? 'https' : 'http';
  var port = '';

  // cache busting is forced
  if (false !== this.timestampRequests) {
    query[this.timestampParam] = +new Date + '-' + Transport.timestamps++;
  }

  if (!this.supportsBinary && !query.sid) {
    query.b64 = 1;
  }

  query = parseqs.encode(query);

  // avoid port if default for schema
  if (this.port && (('https' == schema && this.port != 443) ||
     ('http' == schema && this.port != 80))) {
    port = ':' + this.port;
  }

  // prepend ? to query
  if (query.length) {
    query = '?' + query;
  }

  return schema + '://' + this.hostname + port + this.path + query;
};

},{"../transport":14,"component-inherit":21,"debug":22,"engine.io-parser":25,"parseqs":35,"xmlhttprequest":20}],19:[function(_dereq_,module,exports){
/**
 * Module dependencies.
 */

var Transport = _dereq_('../transport');
var parser = _dereq_('engine.io-parser');
var parseqs = _dereq_('parseqs');
var inherit = _dereq_('component-inherit');
var debug = _dereq_('debug')('engine.io-client:websocket');

/**
 * `ws` exposes a WebSocket-compatible interface in
 * Node, or the `WebSocket` or `MozWebSocket` globals
 * in the browser.
 */

var WebSocket = _dereq_('ws');

/**
 * Module exports.
 */

module.exports = WS;

/**
 * WebSocket transport constructor.
 *
 * @api {Object} connection options
 * @api public
 */

function WS(opts){
  var forceBase64 = (opts && opts.forceBase64);
  if (forceBase64) {
    this.supportsBinary = false;
  }
  Transport.call(this, opts);
}

/**
 * Inherits from Transport.
 */

inherit(WS, Transport);

/**
 * Transport name.
 *
 * @api public
 */

WS.prototype.name = 'websocket';

/*
 * WebSockets support binary
 */

WS.prototype.supportsBinary = true;

/**
 * Opens socket.
 *
 * @api private
 */

WS.prototype.doOpen = function(){
  if (!this.check()) {
    // let probe timeout
    return;
  }

  var self = this;
  var uri = this.uri();
  var protocols = void(0);
  var opts = { agent: this.agent };

  // SSL options for Node.js client
  opts.pfx = this.pfx;
  opts.key = this.key;
  opts.passphrase = this.passphrase;
  opts.cert = this.cert;
  opts.ca = this.ca;
  opts.ciphers = this.ciphers;
  opts.rejectUnauthorized = this.rejectUnauthorized;

  this.ws = new WebSocket(uri, protocols, opts);

  if (this.ws.binaryType === undefined) {
    this.supportsBinary = false;
  }

  this.ws.binaryType = 'arraybuffer';
  this.addEventListeners();
};

/**
 * Adds event listeners to the socket
 *
 * @api private
 */

WS.prototype.addEventListeners = function(){
  var self = this;

  this.ws.onopen = function(){
    self.onOpen();
  };
  this.ws.onclose = function(){
    self.onClose();
  };
  this.ws.onmessage = function(ev){
    self.onData(ev.data);
  };
  this.ws.onerror = function(e){
    self.onError('websocket error', e);
  };
};

/**
 * Override `onData` to use a timer on iOS.
 * See: https://gist.github.com/mloughran/2052006
 *
 * @api private
 */

if ('undefined' != typeof navigator
  && /iPad|iPhone|iPod/i.test(navigator.userAgent)) {
  WS.prototype.onData = function(data){
    var self = this;
    setTimeout(function(){
      Transport.prototype.onData.call(self, data);
    }, 0);
  };
}

/**
 * Writes data to socket.
 *
 * @param {Array} array of packets.
 * @api private
 */

WS.prototype.write = function(packets){
  var self = this;
  this.writable = false;
  // encodePacket efficient as it uses WS framing
  // no need for encodePayload
  for (var i = 0, l = packets.length; i < l; i++) {
    parser.encodePacket(packets[i], this.supportsBinary, function(data) {
      //Sometimes the websocket has already been closed but the browser didn't
      //have a chance of informing us about it yet, in that case send will
      //throw an error
      try {
        self.ws.send(data);
      } catch (e){
        debug('websocket closed before onclose event');
      }
    });
  }

  function ondrain() {
    self.writable = true;
    self.emit('drain');
  }
  // fake drain
  // defer to next tick to allow Socket to clear writeBuffer
  setTimeout(ondrain, 0);
};

/**
 * Called upon close
 *
 * @api private
 */

WS.prototype.onClose = function(){
  Transport.prototype.onClose.call(this);
};

/**
 * Closes socket.
 *
 * @api private
 */

WS.prototype.doClose = function(){
  if (typeof this.ws !== 'undefined') {
    this.ws.close();
  }
};

/**
 * Generates uri for connection.
 *
 * @api private
 */

WS.prototype.uri = function(){
  var query = this.query || {};
  var schema = this.secure ? 'wss' : 'ws';
  var port = '';

  // avoid port if default for schema
  if (this.port && (('wss' == schema && this.port != 443)
    || ('ws' == schema && this.port != 80))) {
    port = ':' + this.port;
  }

  // append timestamp to URI
  if (this.timestampRequests) {
    query[this.timestampParam] = +new Date;
  }

  // communicate binary support capabilities
  if (!this.supportsBinary) {
    query.b64 = 1;
  }

  query = parseqs.encode(query);

  // prepend ? to query
  if (query.length) {
    query = '?' + query;
  }

  return schema + '://' + this.hostname + port + this.path + query;
};

/**
 * Feature detection for WebSocket.
 *
 * @return {Boolean} whether this transport is available.
 * @api public
 */

WS.prototype.check = function(){
  return !!WebSocket && !('__initialize' in WebSocket && this.name === WS.prototype.name);
};

},{"../transport":14,"component-inherit":21,"debug":22,"engine.io-parser":25,"parseqs":35,"ws":37}],20:[function(_dereq_,module,exports){
// browser shim for xmlhttprequest module
var hasCORS = _dereq_('has-cors');

module.exports = function(opts) {
  var xdomain = opts.xdomain;

  // scheme must be same when usign XDomainRequest
  // http://blogs.msdn.com/b/ieinternals/archive/2010/05/13/xdomainrequest-restrictions-limitations-and-workarounds.aspx
  var xscheme = opts.xscheme;

  // XDomainRequest has a flow of not sending cookie, therefore it should be disabled as a default.
  // https://github.com/Automattic/engine.io-client/pull/217
  var enablesXDR = opts.enablesXDR;

  // XMLHttpRequest can be disabled on IE
  try {
    if ('undefined' != typeof XMLHttpRequest && (!xdomain || hasCORS)) {
      return new XMLHttpRequest();
    }
  } catch (e) { }

  // Use XDomainRequest for IE8 if enablesXDR is true
  // because loading bar keeps flashing when using jsonp-polling
  // https://github.com/yujiosaka/socke.io-ie8-loading-example
  try {
    if ('undefined' != typeof XDomainRequest && !xscheme && enablesXDR) {
      return new XDomainRequest();
    }
  } catch (e) { }

  if (!xdomain) {
    try {
      return new ActiveXObject('Microsoft.XMLHTTP');
    } catch(e) { }
  }
}

},{"has-cors":40}],21:[function(_dereq_,module,exports){

module.exports = function(a, b){
  var fn = function(){};
  fn.prototype = b.prototype;
  a.prototype = new fn;
  a.prototype.constructor = a;
};
},{}],22:[function(_dereq_,module,exports){

/**
 * This is the web browser implementation of `debug()`.
 *
 * Expose `debug()` as the module.
 */

exports = module.exports = _dereq_('./debug');
exports.log = log;
exports.formatArgs = formatArgs;
exports.save = save;
exports.load = load;
exports.useColors = useColors;

/**
 * Colors.
 */

exports.colors = [
  'lightseagreen',
  'forestgreen',
  'goldenrod',
  'dodgerblue',
  'darkorchid',
  'crimson'
];

/**
 * Currently only WebKit-based Web Inspectors, Firefox >= v31,
 * and the Firebug extension (any Firefox version) are known
 * to support "%c" CSS customizations.
 *
 * TODO: add a `localStorage` variable to explicitly enable/disable colors
 */

function useColors() {
  // is webkit? http://stackoverflow.com/a/16459606/376773
  return ('WebkitAppearance' in document.documentElement.style) ||
    // is firebug? http://stackoverflow.com/a/398120/376773
    (window.console && (console.firebug || (console.exception && console.table))) ||
    // is firefox >= v31?
    // https://developer.mozilla.org/en-US/docs/Tools/Web_Console#Styling_messages
    (navigator.userAgent.toLowerCase().match(/firefox\/(\d+)/) && parseInt(RegExp.$1, 10) >= 31);
}

/**
 * Map %j to `JSON.stringify()`, since no Web Inspectors do that by default.
 */

exports.formatters.j = function(v) {
  return JSON.stringify(v);
};


/**
 * Colorize log arguments if enabled.
 *
 * @api public
 */

function formatArgs() {
  var args = arguments;
  var useColors = this.useColors;

  args[0] = (useColors ? '%c' : '')
    + this.namespace
    + (useColors ? ' %c' : ' ')
    + args[0]
    + (useColors ? '%c ' : ' ')
    + '+' + exports.humanize(this.diff);

  if (!useColors) return args;

  var c = 'color: ' + this.color;
  args = [args[0], c, 'color: inherit'].concat(Array.prototype.slice.call(args, 1));

  // the final "%c" is somewhat tricky, because there could be other
  // arguments passed either before or after the %c, so we need to
  // figure out the correct index to insert the CSS into
  var index = 0;
  var lastC = 0;
  args[0].replace(/%[a-z%]/g, function(match) {
    if ('%' === match) return;
    index++;
    if ('%c' === match) {
      // we only are interested in the *last* %c
      // (the user may have provided their own)
      lastC = index;
    }
  });

  args.splice(lastC, 0, c);
  return args;
}

/**
 * Invokes `console.log()` when available.
 * No-op when `console.log` is not a "function".
 *
 * @api public
 */

function log() {
  // This hackery is required for IE8,
  // where the `console.log` function doesn't have 'apply'
  return 'object' == typeof console
    && 'function' == typeof console.log
    && Function.prototype.apply.call(console.log, console, arguments);
}

/**
 * Save `namespaces`.
 *
 * @param {String} namespaces
 * @api private
 */

function save(namespaces) {
  try {
    if (null == namespaces) {
      localStorage.removeItem('debug');
    } else {
      localStorage.debug = namespaces;
    }
  } catch(e) {}
}

/**
 * Load `namespaces`.
 *
 * @return {String} returns the previously persisted debug modes
 * @api private
 */

function load() {
  var r;
  try {
    r = localStorage.debug;
  } catch(e) {}
  return r;
}

/**
 * Enable namespaces listed in `localStorage.debug` initially.
 */

exports.enable(load());

},{"./debug":23}],23:[function(_dereq_,module,exports){

/**
 * This is the common logic for both the Node.js and web browser
 * implementations of `debug()`.
 *
 * Expose `debug()` as the module.
 */

exports = module.exports = debug;
exports.coerce = coerce;
exports.disable = disable;
exports.enable = enable;
exports.enabled = enabled;
exports.humanize = _dereq_('ms');

/**
 * The currently active debug mode names, and names to skip.
 */

exports.names = [];
exports.skips = [];

/**
 * Map of special "%n" handling functions, for the debug "format" argument.
 *
 * Valid key names are a single, lowercased letter, i.e. "n".
 */

exports.formatters = {};

/**
 * Previously assigned color.
 */

var prevColor = 0;

/**
 * Previous log timestamp.
 */

var prevTime;

/**
 * Select a color.
 *
 * @return {Number}
 * @api private
 */

function selectColor() {
  return exports.colors[prevColor++ % exports.colors.length];
}

/**
 * Create a debugger with the given `namespace`.
 *
 * @param {String} namespace
 * @return {Function}
 * @api public
 */

function debug(namespace) {

  // define the `disabled` version
  function disabled() {
  }
  disabled.enabled = false;

  // define the `enabled` version
  function enabled() {

    var self = enabled;

    // set `diff` timestamp
    var curr = +new Date();
    var ms = curr - (prevTime || curr);
    self.diff = ms;
    self.prev = prevTime;
    self.curr = curr;
    prevTime = curr;

    // add the `color` if not set
    if (null == self.useColors) self.useColors = exports.useColors();
    if (null == self.color && self.useColors) self.color = selectColor();

    var args = Array.prototype.slice.call(arguments);

    args[0] = exports.coerce(args[0]);

    if ('string' !== typeof args[0]) {
      // anything else let's inspect with %o
      args = ['%o'].concat(args);
    }

    // apply any `formatters` transformations
    var index = 0;
    args[0] = args[0].replace(/%([a-z%])/g, function(match, format) {
      // if we encounter an escaped % then don't increase the array index
      if (match === '%') return match;
      index++;
      var formatter = exports.formatters[format];
      if ('function' === typeof formatter) {
        var val = args[index];
        match = formatter.call(self, val);

        // now we need to remove `args[index]` since it's inlined in the `format`
        args.splice(index, 1);
        index--;
      }
      return match;
    });

    if ('function' === typeof exports.formatArgs) {
      args = exports.formatArgs.apply(self, args);
    }
    var logFn = enabled.log || exports.log || console.log.bind(console);
    logFn.apply(self, args);
  }
  enabled.enabled = true;

  var fn = exports.enabled(namespace) ? enabled : disabled;

  fn.namespace = namespace;

  return fn;
}

/**
 * Enables a debug mode by namespaces. This can include modes
 * separated by a colon and wildcards.
 *
 * @param {String} namespaces
 * @api public
 */

function enable(namespaces) {
  exports.save(namespaces);

  var split = (namespaces || '').split(/[\s,]+/);
  var len = split.length;

  for (var i = 0; i < len; i++) {
    if (!split[i]) continue; // ignore empty strings
    namespaces = split[i].replace(/\*/g, '.*?');
    if (namespaces[0] === '-') {
      exports.skips.push(new RegExp('^' + namespaces.substr(1) + '$'));
    } else {
      exports.names.push(new RegExp('^' + namespaces + '$'));
    }
  }
}

/**
 * Disable debug output.
 *
 * @api public
 */

function disable() {
  exports.enable('');
}

/**
 * Returns true if the given mode name is enabled, false otherwise.
 *
 * @param {String} name
 * @return {Boolean}
 * @api public
 */

function enabled(name) {
  var i, len;
  for (i = 0, len = exports.skips.length; i < len; i++) {
    if (exports.skips[i].test(name)) {
      return false;
    }
  }
  for (i = 0, len = exports.names.length; i < len; i++) {
    if (exports.names[i].test(name)) {
      return true;
    }
  }
  return false;
}

/**
 * Coerce `val`.
 *
 * @param {Mixed} val
 * @return {Mixed}
 * @api private
 */

function coerce(val) {
  if (val instanceof Error) return val.stack || val.message;
  return val;
}

},{"ms":24}],24:[function(_dereq_,module,exports){
/**
 * Helpers.
 */

var s = 1000;
var m = s * 60;
var h = m * 60;
var d = h * 24;
var y = d * 365.25;

/**
 * Parse or format the given `val`.
 *
 * Options:
 *
 *  - `long` verbose formatting [false]
 *
 * @param {String|Number} val
 * @param {Object} options
 * @return {String|Number}
 * @api public
 */

module.exports = function(val, options){
  options = options || {};
  if ('string' == typeof val) return parse(val);
  return options.long
    ? long(val)
    : short(val);
};

/**
 * Parse the given `str` and return milliseconds.
 *
 * @param {String} str
 * @return {Number}
 * @api private
 */

function parse(str) {
  var match = /^((?:\d+)?\.?\d+) *(ms|seconds?|s|minutes?|m|hours?|h|days?|d|years?|y)?$/i.exec(str);
  if (!match) return;
  var n = parseFloat(match[1]);
  var type = (match[2] || 'ms').toLowerCase();
  switch (type) {
    case 'years':
    case 'year':
    case 'y':
      return n * y;
    case 'days':
    case 'day':
    case 'd':
      return n * d;
    case 'hours':
    case 'hour':
    case 'h':
      return n * h;
    case 'minutes':
    case 'minute':
    case 'm':
      return n * m;
    case 'seconds':
    case 'second':
    case 's':
      return n * s;
    case 'ms':
      return n;
  }
}

/**
 * Short format for `ms`.
 *
 * @param {Number} ms
 * @return {String}
 * @api private
 */

function short(ms) {
  if (ms >= d) return Math.round(ms / d) + 'd';
  if (ms >= h) return Math.round(ms / h) + 'h';
  if (ms >= m) return Math.round(ms / m) + 'm';
  if (ms >= s) return Math.round(ms / s) + 's';
  return ms + 'ms';
}

/**
 * Long format for `ms`.
 *
 * @param {Number} ms
 * @return {String}
 * @api private
 */

function long(ms) {
  return plural(ms, d, 'day')
    || plural(ms, h, 'hour')
    || plural(ms, m, 'minute')
    || plural(ms, s, 'second')
    || ms + ' ms';
}

/**
 * Pluralization helper.
 */

function plural(ms, n, name) {
  if (ms < n) return;
  if (ms < n * 1.5) return Math.floor(ms / n) + ' ' + name;
  return Math.ceil(ms / n) + ' ' + name + 's';
}

},{}],25:[function(_dereq_,module,exports){
(function (global){
/**
 * Module dependencies.
 */

var keys = _dereq_('./keys');
var hasBinary = _dereq_('has-binary');
var sliceBuffer = _dereq_('arraybuffer.slice');
var base64encoder = _dereq_('base64-arraybuffer');
var after = _dereq_('after');
var utf8 = _dereq_('utf8');

/**
 * Check if we are running an android browser. That requires us to use
 * ArrayBuffer with polling transports...
 *
 * http://ghinda.net/jpeg-blob-ajax-android/
 */

var isAndroid = navigator.userAgent.match(/Android/i);

/**
 * Check if we are running in PhantomJS.
 * Uploading a Blob with PhantomJS does not work correctly, as reported here:
 * https://github.com/ariya/phantomjs/issues/11395
 * @type boolean
 */
var isPhantomJS = /PhantomJS/i.test(navigator.userAgent);

/**
 * When true, avoids using Blobs to encode payloads.
 * @type boolean
 */
var dontSendBlobs = isAndroid || isPhantomJS;

/**
 * Current protocol version.
 */

exports.protocol = 3;

/**
 * Packet types.
 */

var packets = exports.packets = {
    open:     0    // non-ws
  , close:    1    // non-ws
  , ping:     2
  , pong:     3
  , message:  4
  , upgrade:  5
  , noop:     6
};

var packetslist = keys(packets);

/**
 * Premade error packet.
 */

var err = { type: 'error', data: 'parser error' };

/**
 * Create a blob api even for blob builder when vendor prefixes exist
 */

var Blob = _dereq_('blob');

/**
 * Encodes a packet.
 *
 *     <packet type id> [ <data> ]
 *
 * Example:
 *
 *     5hello world
 *     3
 *     4
 *
 * Binary is encoded in an identical principle
 *
 * @api private
 */

exports.encodePacket = function (packet, supportsBinary, utf8encode, callback) {
  if ('function' == typeof supportsBinary) {
    callback = supportsBinary;
    supportsBinary = false;
  }

  if ('function' == typeof utf8encode) {
    callback = utf8encode;
    utf8encode = null;
  }

  var data = (packet.data === undefined)
    ? undefined
    : packet.data.buffer || packet.data;

  if (global.ArrayBuffer && data instanceof ArrayBuffer) {
    return encodeArrayBuffer(packet, supportsBinary, callback);
  } else if (Blob && data instanceof global.Blob) {
    return encodeBlob(packet, supportsBinary, callback);
  }

  // might be an object with { base64: true, data: dataAsBase64String }
  if (data && data.base64) {
    return encodeBase64Object(packet, callback);
  }

  // Sending data as a utf-8 string
  var encoded = packets[packet.type];

  // data fragment is optional
  if (undefined !== packet.data) {
    encoded += utf8encode ? utf8.encode(String(packet.data)) : String(packet.data);
  }

  return callback('' + encoded);

};

function encodeBase64Object(packet, callback) {
  // packet data is an object { base64: true, data: dataAsBase64String }
  var message = 'b' + exports.packets[packet.type] + packet.data.data;
  return callback(message);
}

/**
 * Encode packet helpers for binary types
 */

function encodeArrayBuffer(packet, supportsBinary, callback) {
  if (!supportsBinary) {
    return exports.encodeBase64Packet(packet, callback);
  }

  var data = packet.data;
  var contentArray = new Uint8Array(data);
  var resultBuffer = new Uint8Array(1 + data.byteLength);

  resultBuffer[0] = packets[packet.type];
  for (var i = 0; i < contentArray.length; i++) {
    resultBuffer[i+1] = contentArray[i];
  }

  return callback(resultBuffer.buffer);
}

function encodeBlobAsArrayBuffer(packet, supportsBinary, callback) {
  if (!supportsBinary) {
    return exports.encodeBase64Packet(packet, callback);
  }

  var fr = new FileReader();
  fr.onload = function() {
    packet.data = fr.result;
    exports.encodePacket(packet, supportsBinary, true, callback);
  };
  return fr.readAsArrayBuffer(packet.data);
}

function encodeBlob(packet, supportsBinary, callback) {
  if (!supportsBinary) {
    return exports.encodeBase64Packet(packet, callback);
  }

  if (dontSendBlobs) {
    return encodeBlobAsArrayBuffer(packet, supportsBinary, callback);
  }

  var length = new Uint8Array(1);
  length[0] = packets[packet.type];
  var blob = new Blob([length.buffer, packet.data]);

  return callback(blob);
}

/**
 * Encodes a packet with binary data in a base64 string
 *
 * @param {Object} packet, has `type` and `data`
 * @return {String} base64 encoded message
 */

exports.encodeBase64Packet = function(packet, callback) {
  var message = 'b' + exports.packets[packet.type];
  if (Blob && packet.data instanceof Blob) {
    var fr = new FileReader();
    fr.onload = function() {
      var b64 = fr.result.split(',')[1];
      callback(message + b64);
    };
    return fr.readAsDataURL(packet.data);
  }

  var b64data;
  try {
    b64data = String.fromCharCode.apply(null, new Uint8Array(packet.data));
  } catch (e) {
    // iPhone Safari doesn't let you apply with typed arrays
    var typed = new Uint8Array(packet.data);
    var basic = new Array(typed.length);
    for (var i = 0; i < typed.length; i++) {
      basic[i] = typed[i];
    }
    b64data = String.fromCharCode.apply(null, basic);
  }
  message += global.btoa(b64data);
  return callback(message);
};

/**
 * Decodes a packet. Changes format to Blob if requested.
 *
 * @return {Object} with `type` and `data` (if any)
 * @api private
 */

exports.decodePacket = function (data, binaryType, utf8decode) {
  // String data
  if (typeof data == 'string' || data === undefined) {
    if (data.charAt(0) == 'b') {
      return exports.decodeBase64Packet(data.substr(1), binaryType);
    }

    if (utf8decode) {
      try {
        data = utf8.decode(data);
      } catch (e) {
        return err;
      }
    }
    var type = data.charAt(0);

    if (Number(type) != type || !packetslist[type]) {
      return err;
    }

    if (data.length > 1) {
      return { type: packetslist[type], data: data.substring(1) };
    } else {
      return { type: packetslist[type] };
    }
  }

  var asArray = new Uint8Array(data);
  var type = asArray[0];
  var rest = sliceBuffer(data, 1);
  if (Blob && binaryType === 'blob') {
    rest = new Blob([rest]);
  }
  return { type: packetslist[type], data: rest };
};

/**
 * Decodes a packet encoded in a base64 string
 *
 * @param {String} base64 encoded message
 * @return {Object} with `type` and `data` (if any)
 */

exports.decodeBase64Packet = function(msg, binaryType) {
  var type = packetslist[msg.charAt(0)];
  if (!global.ArrayBuffer) {
    return { type: type, data: { base64: true, data: msg.substr(1) } };
  }

  var data = base64encoder.decode(msg.substr(1));

  if (binaryType === 'blob' && Blob) {
    data = new Blob([data]);
  }

  return { type: type, data: data };
};

/**
 * Encodes multiple messages (payload).
 *
 *     <length>:data
 *
 * Example:
 *
 *     11:hello world2:hi
 *
 * If any contents are binary, they will be encoded as base64 strings. Base64
 * encoded strings are marked with a b before the length specifier
 *
 * @param {Array} packets
 * @api private
 */

exports.encodePayload = function (packets, supportsBinary, callback) {
  if (typeof supportsBinary == 'function') {
    callback = supportsBinary;
    supportsBinary = null;
  }

  var isBinary = hasBinary(packets);

  if (supportsBinary && isBinary) {
    if (Blob && !dontSendBlobs) {
      return exports.encodePayloadAsBlob(packets, callback);
    }

    return exports.encodePayloadAsArrayBuffer(packets, callback);
  }

  if (!packets.length) {
    return callback('0:');
  }

  function setLengthHeader(message) {
    return message.length + ':' + message;
  }

  function encodeOne(packet, doneCallback) {
    exports.encodePacket(packet, !isBinary ? false : supportsBinary, true, function(message) {
      doneCallback(null, setLengthHeader(message));
    });
  }

  map(packets, encodeOne, function(err, results) {
    return callback(results.join(''));
  });
};

/**
 * Async array map using after
 */

function map(ary, each, done) {
  var result = new Array(ary.length);
  var next = after(ary.length, done);

  var eachWithIndex = function(i, el, cb) {
    each(el, function(error, msg) {
      result[i] = msg;
      cb(error, result);
    });
  };

  for (var i = 0; i < ary.length; i++) {
    eachWithIndex(i, ary[i], next);
  }
}

/*
 * Decodes data when a payload is maybe expected. Possible binary contents are
 * decoded from their base64 representation
 *
 * @param {String} data, callback method
 * @api public
 */

exports.decodePayload = function (data, binaryType, callback) {
  if (typeof data != 'string') {
    return exports.decodePayloadAsBinary(data, binaryType, callback);
  }

  if (typeof binaryType === 'function') {
    callback = binaryType;
    binaryType = null;
  }

  var packet;
  if (data == '') {
    // parser error - ignoring payload
    return callback(err, 0, 1);
  }

  var length = ''
    , n, msg;

  for (var i = 0, l = data.length; i < l; i++) {
    var chr = data.charAt(i);

    if (':' != chr) {
      length += chr;
    } else {
      if ('' == length || (length != (n = Number(length)))) {
        // parser error - ignoring payload
        return callback(err, 0, 1);
      }

      msg = data.substr(i + 1, n);

      if (length != msg.length) {
        // parser error - ignoring payload
        return callback(err, 0, 1);
      }

      if (msg.length) {
        packet = exports.decodePacket(msg, binaryType, true);

        if (err.type == packet.type && err.data == packet.data) {
          // parser error in individual packet - ignoring payload
          return callback(err, 0, 1);
        }

        var ret = callback(packet, i + n, l);
        if (false === ret) return;
      }

      // advance cursor
      i += n;
      length = '';
    }
  }

  if (length != '') {
    // parser error - ignoring payload
    return callback(err, 0, 1);
  }

};

/**
 * Encodes multiple messages (payload) as binary.
 *
 * <1 = binary, 0 = string><number from 0-9><number from 0-9>[...]<number
 * 255><data>
 *
 * Example:
 * 1 3 255 1 2 3, if the binary contents are interpreted as 8 bit integers
 *
 * @param {Array} packets
 * @return {ArrayBuffer} encoded payload
 * @api private
 */

exports.encodePayloadAsArrayBuffer = function(packets, callback) {
  if (!packets.length) {
    return callback(new ArrayBuffer(0));
  }

  function encodeOne(packet, doneCallback) {
    exports.encodePacket(packet, true, true, function(data) {
      return doneCallback(null, data);
    });
  }

  map(packets, encodeOne, function(err, encodedPackets) {
    var totalLength = encodedPackets.reduce(function(acc, p) {
      var len;
      if (typeof p === 'string'){
        len = p.length;
      } else {
        len = p.byteLength;
      }
      return acc + len.toString().length + len + 2; // string/binary identifier + separator = 2
    }, 0);

    var resultArray = new Uint8Array(totalLength);

    var bufferIndex = 0;
    encodedPackets.forEach(function(p) {
      var isString = typeof p === 'string';
      var ab = p;
      if (isString) {
        var view = new Uint8Array(p.length);
        for (var i = 0; i < p.length; i++) {
          view[i] = p.charCodeAt(i);
        }
        ab = view.buffer;
      }

      if (isString) { // not true binary
        resultArray[bufferIndex++] = 0;
      } else { // true binary
        resultArray[bufferIndex++] = 1;
      }

      var lenStr = ab.byteLength.toString();
      for (var i = 0; i < lenStr.length; i++) {
        resultArray[bufferIndex++] = parseInt(lenStr[i]);
      }
      resultArray[bufferIndex++] = 255;

      var view = new Uint8Array(ab);
      for (var i = 0; i < view.length; i++) {
        resultArray[bufferIndex++] = view[i];
      }
    });

    return callback(resultArray.buffer);
  });
};

/**
 * Encode as Blob
 */

exports.encodePayloadAsBlob = function(packets, callback) {
  function encodeOne(packet, doneCallback) {
    exports.encodePacket(packet, true, true, function(encoded) {
      var binaryIdentifier = new Uint8Array(1);
      binaryIdentifier[0] = 1;
      if (typeof encoded === 'string') {
        var view = new Uint8Array(encoded.length);
        for (var i = 0; i < encoded.length; i++) {
          view[i] = encoded.charCodeAt(i);
        }
        encoded = view.buffer;
        binaryIdentifier[0] = 0;
      }

      var len = (encoded instanceof ArrayBuffer)
        ? encoded.byteLength
        : encoded.size;

      var lenStr = len.toString();
      var lengthAry = new Uint8Array(lenStr.length + 1);
      for (var i = 0; i < lenStr.length; i++) {
        lengthAry[i] = parseInt(lenStr[i]);
      }
      lengthAry[lenStr.length] = 255;

      if (Blob) {
        var blob = new Blob([binaryIdentifier.buffer, lengthAry.buffer, encoded]);
        doneCallback(null, blob);
      }
    });
  }

  map(packets, encodeOne, function(err, results) {
    return callback(new Blob(results));
  });
};

/*
 * Decodes data when a payload is maybe expected. Strings are decoded by
 * interpreting each byte as a key code for entries marked to start with 0. See
 * description of encodePayloadAsBinary
 *
 * @param {ArrayBuffer} data, callback method
 * @api public
 */

exports.decodePayloadAsBinary = function (data, binaryType, callback) {
  if (typeof binaryType === 'function') {
    callback = binaryType;
    binaryType = null;
  }

  var bufferTail = data;
  var buffers = [];

  var numberTooLong = false;
  while (bufferTail.byteLength > 0) {
    var tailArray = new Uint8Array(bufferTail);
    var isString = tailArray[0] === 0;
    var msgLength = '';

    for (var i = 1; ; i++) {
      if (tailArray[i] == 255) break;

      if (msgLength.length > 310) {
        numberTooLong = true;
        break;
      }

      msgLength += tailArray[i];
    }

    if(numberTooLong) return callback(err, 0, 1);

    bufferTail = sliceBuffer(bufferTail, 2 + msgLength.length);
    msgLength = parseInt(msgLength);

    var msg = sliceBuffer(bufferTail, 0, msgLength);
    if (isString) {
      try {
        msg = String.fromCharCode.apply(null, new Uint8Array(msg));
      } catch (e) {
        // iPhone Safari doesn't let you apply to typed arrays
        var typed = new Uint8Array(msg);
        msg = '';
        for (var i = 0; i < typed.length; i++) {
          msg += String.fromCharCode(typed[i]);
        }
      }
    }

    buffers.push(msg);
    bufferTail = sliceBuffer(bufferTail, msgLength);
  }

  var total = buffers.length;
  buffers.forEach(function(buffer, i) {
    callback(exports.decodePacket(buffer, binaryType, true), i, total);
  });
};

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./keys":26,"after":27,"arraybuffer.slice":28,"base64-arraybuffer":29,"blob":30,"has-binary":31,"utf8":33}],26:[function(_dereq_,module,exports){

/**
 * Gets the keys for an object.
 *
 * @return {Array} keys
 * @api private
 */

module.exports = Object.keys || function keys (obj){
  var arr = [];
  var has = Object.prototype.hasOwnProperty;

  for (var i in obj) {
    if (has.call(obj, i)) {
      arr.push(i);
    }
  }
  return arr;
};

},{}],27:[function(_dereq_,module,exports){
module.exports = after

function after(count, callback, err_cb) {
    var bail = false
    err_cb = err_cb || noop
    proxy.count = count

    return (count === 0) ? callback() : proxy

    function proxy(err, result) {
        if (proxy.count <= 0) {
            throw new Error('after called too many times')
        }
        --proxy.count

        // after first error, rest are passed to err_cb
        if (err) {
            bail = true
            callback(err)
            // future error callbacks will go to error handler
            callback = err_cb
        } else if (proxy.count === 0 && !bail) {
            callback(null, result)
        }
    }
}

function noop() {}

},{}],28:[function(_dereq_,module,exports){
/**
 * An abstraction for slicing an arraybuffer even when
 * ArrayBuffer.prototype.slice is not supported
 *
 * @api public
 */

module.exports = function(arraybuffer, start, end) {
  var bytes = arraybuffer.byteLength;
  start = start || 0;
  end = end || bytes;

  if (arraybuffer.slice) { return arraybuffer.slice(start, end); }

  if (start < 0) { start += bytes; }
  if (end < 0) { end += bytes; }
  if (end > bytes) { end = bytes; }

  if (start >= bytes || start >= end || bytes === 0) {
    return new ArrayBuffer(0);
  }

  var abv = new Uint8Array(arraybuffer);
  var result = new Uint8Array(end - start);
  for (var i = start, ii = 0; i < end; i++, ii++) {
    result[ii] = abv[i];
  }
  return result.buffer;
};

},{}],29:[function(_dereq_,module,exports){
/*
 * base64-arraybuffer
 * https://github.com/niklasvh/base64-arraybuffer
 *
 * Copyright (c) 2012 Niklas von Hertzen
 * Licensed under the MIT license.
 */
(function(chars){
  "use strict";

  exports.encode = function(arraybuffer) {
    var bytes = new Uint8Array(arraybuffer),
    i, len = bytes.length, base64 = "";

    for (i = 0; i < len; i+=3) {
      base64 += chars[bytes[i] >> 2];
      base64 += chars[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
      base64 += chars[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
      base64 += chars[bytes[i + 2] & 63];
    }

    if ((len % 3) === 2) {
      base64 = base64.substring(0, base64.length - 1) + "=";
    } else if (len % 3 === 1) {
      base64 = base64.substring(0, base64.length - 2) + "==";
    }

    return base64;
  };

  exports.decode =  function(base64) {
    var bufferLength = base64.length * 0.75,
    len = base64.length, i, p = 0,
    encoded1, encoded2, encoded3, encoded4;

    if (base64[base64.length - 1] === "=") {
      bufferLength--;
      if (base64[base64.length - 2] === "=") {
        bufferLength--;
      }
    }

    var arraybuffer = new ArrayBuffer(bufferLength),
    bytes = new Uint8Array(arraybuffer);

    for (i = 0; i < len; i+=4) {
      encoded1 = chars.indexOf(base64[i]);
      encoded2 = chars.indexOf(base64[i+1]);
      encoded3 = chars.indexOf(base64[i+2]);
      encoded4 = chars.indexOf(base64[i+3]);

      bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
      bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
      bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
    }

    return arraybuffer;
  };
})("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");

},{}],30:[function(_dereq_,module,exports){
(function (global){
/**
 * Create a blob builder even when vendor prefixes exist
 */

var BlobBuilder = global.BlobBuilder
  || global.WebKitBlobBuilder
  || global.MSBlobBuilder
  || global.MozBlobBuilder;

/**
 * Check if Blob constructor is supported
 */

var blobSupported = (function() {
  try {
    var b = new Blob(['hi']);
    return b.size == 2;
  } catch(e) {
    return false;
  }
})();

/**
 * Check if BlobBuilder is supported
 */

var blobBuilderSupported = BlobBuilder
  && BlobBuilder.prototype.append
  && BlobBuilder.prototype.getBlob;

function BlobBuilderConstructor(ary, options) {
  options = options || {};

  var bb = new BlobBuilder();
  for (var i = 0; i < ary.length; i++) {
    bb.append(ary[i]);
  }
  return (options.type) ? bb.getBlob(options.type) : bb.getBlob();
};

module.exports = (function() {
  if (blobSupported) {
    return global.Blob;
  } else if (blobBuilderSupported) {
    return BlobBuilderConstructor;
  } else {
    return undefined;
  }
})();

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],31:[function(_dereq_,module,exports){
(function (global){

/*
 * Module requirements.
 */

var isArray = _dereq_('isarray');

/**
 * Module exports.
 */

module.exports = hasBinary;

/**
 * Checks for binary data.
 *
 * Right now only Buffer and ArrayBuffer are supported..
 *
 * @param {Object} anything
 * @api public
 */

function hasBinary(data) {

  function _hasBinary(obj) {
    if (!obj) return false;

    if ( (global.Buffer && global.Buffer.isBuffer(obj)) ||
         (global.ArrayBuffer && obj instanceof ArrayBuffer) ||
         (global.Blob && obj instanceof Blob) ||
         (global.File && obj instanceof File)
        ) {
      return true;
    }

    if (isArray(obj)) {
      for (var i = 0; i < obj.length; i++) {
          if (_hasBinary(obj[i])) {
              return true;
          }
      }
    } else if (obj && 'object' == typeof obj) {
      if (obj.toJSON) {
        obj = obj.toJSON();
      }

      for (var key in obj) {
        if (obj.hasOwnProperty(key) && _hasBinary(obj[key])) {
          return true;
        }
      }
    }

    return false;
  }

  return _hasBinary(data);
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"isarray":32}],32:[function(_dereq_,module,exports){
module.exports = Array.isArray || function (arr) {
  return Object.prototype.toString.call(arr) == '[object Array]';
};

},{}],33:[function(_dereq_,module,exports){
(function (global){
/*! http://mths.be/utf8js v2.0.0 by @mathias */
;(function(root) {

	// Detect free variables `exports`
	var freeExports = typeof exports == 'object' && exports;

	// Detect free variable `module`
	var freeModule = typeof module == 'object' && module &&
		module.exports == freeExports && module;

	// Detect free variable `global`, from Node.js or Browserified code,
	// and use it as `root`
	var freeGlobal = typeof global == 'object' && global;
	if (freeGlobal.global === freeGlobal || freeGlobal.window === freeGlobal) {
		root = freeGlobal;
	}

	/*--------------------------------------------------------------------------*/

	var stringFromCharCode = String.fromCharCode;

	// Taken from http://mths.be/punycode
	function ucs2decode(string) {
		var output = [];
		var counter = 0;
		var length = string.length;
		var value;
		var extra;
		while (counter < length) {
			value = string.charCodeAt(counter++);
			if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
				// high surrogate, and there is a next character
				extra = string.charCodeAt(counter++);
				if ((extra & 0xFC00) == 0xDC00) { // low surrogate
					output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
				} else {
					// unmatched surrogate; only append this code unit, in case the next
					// code unit is the high surrogate of a surrogate pair
					output.push(value);
					counter--;
				}
			} else {
				output.push(value);
			}
		}
		return output;
	}

	// Taken from http://mths.be/punycode
	function ucs2encode(array) {
		var length = array.length;
		var index = -1;
		var value;
		var output = '';
		while (++index < length) {
			value = array[index];
			if (value > 0xFFFF) {
				value -= 0x10000;
				output += stringFromCharCode(value >>> 10 & 0x3FF | 0xD800);
				value = 0xDC00 | value & 0x3FF;
			}
			output += stringFromCharCode(value);
		}
		return output;
	}

	/*--------------------------------------------------------------------------*/

	function createByte(codePoint, shift) {
		return stringFromCharCode(((codePoint >> shift) & 0x3F) | 0x80);
	}

	function encodeCodePoint(codePoint) {
		if ((codePoint & 0xFFFFFF80) == 0) { // 1-byte sequence
			return stringFromCharCode(codePoint);
		}
		var symbol = '';
		if ((codePoint & 0xFFFFF800) == 0) { // 2-byte sequence
			symbol = stringFromCharCode(((codePoint >> 6) & 0x1F) | 0xC0);
		}
		else if ((codePoint & 0xFFFF0000) == 0) { // 3-byte sequence
			symbol = stringFromCharCode(((codePoint >> 12) & 0x0F) | 0xE0);
			symbol += createByte(codePoint, 6);
		}
		else if ((codePoint & 0xFFE00000) == 0) { // 4-byte sequence
			symbol = stringFromCharCode(((codePoint >> 18) & 0x07) | 0xF0);
			symbol += createByte(codePoint, 12);
			symbol += createByte(codePoint, 6);
		}
		symbol += stringFromCharCode((codePoint & 0x3F) | 0x80);
		return symbol;
	}

	function utf8encode(string) {
		var codePoints = ucs2decode(string);

		// console.log(JSON.stringify(codePoints.map(function(x) {
		// 	return 'U+' + x.toString(16).toUpperCase();
		// })));

		var length = codePoints.length;
		var index = -1;
		var codePoint;
		var byteString = '';
		while (++index < length) {
			codePoint = codePoints[index];
			byteString += encodeCodePoint(codePoint);
		}
		return byteString;
	}

	/*--------------------------------------------------------------------------*/

	function readContinuationByte() {
		if (byteIndex >= byteCount) {
			throw Error('Invalid byte index');
		}

		var continuationByte = byteArray[byteIndex] & 0xFF;
		byteIndex++;

		if ((continuationByte & 0xC0) == 0x80) {
			return continuationByte & 0x3F;
		}

		// If we end up here, it’s not a continuation byte
		throw Error('Invalid continuation byte');
	}

	function decodeSymbol() {
		var byte1;
		var byte2;
		var byte3;
		var byte4;
		var codePoint;

		if (byteIndex > byteCount) {
			throw Error('Invalid byte index');
		}

		if (byteIndex == byteCount) {
			return false;
		}

		// Read first byte
		byte1 = byteArray[byteIndex] & 0xFF;
		byteIndex++;

		// 1-byte sequence (no continuation bytes)
		if ((byte1 & 0x80) == 0) {
			return byte1;
		}

		// 2-byte sequence
		if ((byte1 & 0xE0) == 0xC0) {
			var byte2 = readContinuationByte();
			codePoint = ((byte1 & 0x1F) << 6) | byte2;
			if (codePoint >= 0x80) {
				return codePoint;
			} else {
				throw Error('Invalid continuation byte');
			}
		}

		// 3-byte sequence (may include unpaired surrogates)
		if ((byte1 & 0xF0) == 0xE0) {
			byte2 = readContinuationByte();
			byte3 = readContinuationByte();
			codePoint = ((byte1 & 0x0F) << 12) | (byte2 << 6) | byte3;
			if (codePoint >= 0x0800) {
				return codePoint;
			} else {
				throw Error('Invalid continuation byte');
			}
		}

		// 4-byte sequence
		if ((byte1 & 0xF8) == 0xF0) {
			byte2 = readContinuationByte();
			byte3 = readContinuationByte();
			byte4 = readContinuationByte();
			codePoint = ((byte1 & 0x0F) << 0x12) | (byte2 << 0x0C) |
				(byte3 << 0x06) | byte4;
			if (codePoint >= 0x010000 && codePoint <= 0x10FFFF) {
				return codePoint;
			}
		}

		throw Error('Invalid UTF-8 detected');
	}

	var byteArray;
	var byteCount;
	var byteIndex;
	function utf8decode(byteString) {
		byteArray = ucs2decode(byteString);
		byteCount = byteArray.length;
		byteIndex = 0;
		var codePoints = [];
		var tmp;
		while ((tmp = decodeSymbol()) !== false) {
			codePoints.push(tmp);
		}
		return ucs2encode(codePoints);
	}

	/*--------------------------------------------------------------------------*/

	var utf8 = {
		'version': '2.0.0',
		'encode': utf8encode,
		'decode': utf8decode
	};

	// Some AMD build optimizers, like r.js, check for specific condition patterns
	// like the following:
	if (
		typeof define == 'function' &&
		typeof define.amd == 'object' &&
		define.amd
	) {
		define(function() {
			return utf8;
		});
	}	else if (freeExports && !freeExports.nodeType) {
		if (freeModule) { // in Node.js or RingoJS v0.8.0+
			freeModule.exports = utf8;
		} else { // in Narwhal or RingoJS v0.7.0-
			var object = {};
			var hasOwnProperty = object.hasOwnProperty;
			for (var key in utf8) {
				hasOwnProperty.call(utf8, key) && (freeExports[key] = utf8[key]);
			}
		}
	} else { // in Rhino or a web browser
		root.utf8 = utf8;
	}

}(this));

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],34:[function(_dereq_,module,exports){
(function (global){
/**
 * JSON parse.
 *
 * @see Based on jQuery#parseJSON (MIT) and JSON2
 * @api private
 */

var rvalidchars = /^[\],:{}\s]*$/;
var rvalidescape = /\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g;
var rvalidtokens = /"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g;
var rvalidbraces = /(?:^|:|,)(?:\s*\[)+/g;
var rtrimLeft = /^\s+/;
var rtrimRight = /\s+$/;

module.exports = function parsejson(data) {
  if ('string' != typeof data || !data) {
    return null;
  }

  data = data.replace(rtrimLeft, '').replace(rtrimRight, '');

  // Attempt to parse using the native JSON parser first
  if (global.JSON && JSON.parse) {
    return JSON.parse(data);
  }

  if (rvalidchars.test(data.replace(rvalidescape, '@')
      .replace(rvalidtokens, ']')
      .replace(rvalidbraces, ''))) {
    return (new Function('return ' + data))();
  }
};
}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],35:[function(_dereq_,module,exports){
/**
 * Compiles a querystring
 * Returns string representation of the object
 *
 * @param {Object}
 * @api private
 */

exports.encode = function (obj) {
  var str = '';

  for (var i in obj) {
    if (obj.hasOwnProperty(i)) {
      if (str.length) str += '&';
      str += encodeURIComponent(i) + '=' + encodeURIComponent(obj[i]);
    }
  }

  return str;
};

/**
 * Parses a simple querystring into an object
 *
 * @param {String} qs
 * @api private
 */

exports.decode = function(qs){
  var qry = {};
  var pairs = qs.split('&');
  for (var i = 0, l = pairs.length; i < l; i++) {
    var pair = pairs[i].split('=');
    qry[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1]);
  }
  return qry;
};

},{}],36:[function(_dereq_,module,exports){
/**
 * Parses an URI
 *
 * @author Steven Levithan <stevenlevithan.com> (MIT license)
 * @api private
 */

var re = /^(?:(?![^:@]+:[^:@\/]*@)(http|https|ws|wss):\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?((?:[a-f0-9]{0,4}:){2,7}[a-f0-9]{0,4}|[^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/;

var parts = [
    'source', 'protocol', 'authority', 'userInfo', 'user', 'password', 'host', 'port', 'relative', 'path', 'directory', 'file', 'query', 'anchor'
];

module.exports = function parseuri(str) {
    var src = str,
        b = str.indexOf('['),
        e = str.indexOf(']');

    if (b != -1 && e != -1) {
        str = str.substring(0, b) + str.substring(b, e).replace(/:/g, ';') + str.substring(e, str.length);
    }

    var m = re.exec(str || ''),
        uri = {},
        i = 14;

    while (i--) {
        uri[parts[i]] = m[i] || '';
    }

    if (b != -1 && e != -1) {
        uri.source = src;
        uri.host = uri.host.substring(1, uri.host.length - 1).replace(/;/g, ':');
        uri.authority = uri.authority.replace('[', '').replace(']', '').replace(/;/g, ':');
        uri.ipv6uri = true;
    }

    return uri;
};

},{}],37:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var global = (function() { return this; })();

/**
 * WebSocket constructor.
 */

var WebSocket = global.WebSocket || global.MozWebSocket;

/**
 * Module exports.
 */

module.exports = WebSocket ? ws : null;

/**
 * WebSocket constructor.
 *
 * The third `opts` options object gets ignored in web browsers, since it's
 * non-standard, and throws a TypeError if passed to the constructor.
 * See: https://github.com/einaros/ws/issues/227
 *
 * @param {String} uri
 * @param {Array} protocols (optional)
 * @param {Object) opts (optional)
 * @api public
 */

function ws(uri, protocols, opts) {
  var instance;
  if (protocols) {
    instance = new WebSocket(uri, protocols);
  } else {
    instance = new WebSocket(uri);
  }
  return instance;
}

if (WebSocket) ws.prototype = WebSocket.prototype;

},{}],38:[function(_dereq_,module,exports){
(function (global){

/*
 * Module requirements.
 */

var isArray = _dereq_('isarray');

/**
 * Module exports.
 */

module.exports = hasBinary;

/**
 * Checks for binary data.
 *
 * Right now only Buffer and ArrayBuffer are supported..
 *
 * @param {Object} anything
 * @api public
 */

function hasBinary(data) {

  function _hasBinary(obj) {
    if (!obj) return false;

    if ( (global.Buffer && global.Buffer.isBuffer(obj)) ||
         (global.ArrayBuffer && obj instanceof ArrayBuffer) ||
         (global.Blob && obj instanceof Blob) ||
         (global.File && obj instanceof File)
        ) {
      return true;
    }

    if (isArray(obj)) {
      for (var i = 0; i < obj.length; i++) {
          if (_hasBinary(obj[i])) {
              return true;
          }
      }
    } else if (obj && 'object' == typeof obj) {
      if (obj.toJSON) {
        obj = obj.toJSON();
      }

      for (var key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key) && _hasBinary(obj[key])) {
          return true;
        }
      }
    }

    return false;
  }

  return _hasBinary(data);
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"isarray":39}],39:[function(_dereq_,module,exports){
module.exports=_dereq_(32)
},{}],40:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var global = _dereq_('global');

/**
 * Module exports.
 *
 * Logic borrowed from Modernizr:
 *
 *   - https://github.com/Modernizr/Modernizr/blob/master/feature-detects/cors.js
 */

try {
  module.exports = 'XMLHttpRequest' in global &&
    'withCredentials' in new global.XMLHttpRequest();
} catch (err) {
  // if XMLHttp support is disabled in IE then it will throw
  // when trying to create
  module.exports = false;
}

},{"global":41}],41:[function(_dereq_,module,exports){

/**
 * Returns `this`. Execute this without a "context" (i.e. without it being
 * attached to an object of the left-hand side), and `this` points to the
 * "global" scope of the current JS execution.
 */

module.exports = (function () { return this; })();

},{}],42:[function(_dereq_,module,exports){

var indexOf = [].indexOf;

module.exports = function(arr, obj){
  if (indexOf) return arr.indexOf(obj);
  for (var i = 0; i < arr.length; ++i) {
    if (arr[i] === obj) return i;
  }
  return -1;
};
},{}],43:[function(_dereq_,module,exports){

/**
 * HOP ref.
 */

var has = Object.prototype.hasOwnProperty;

/**
 * Return own keys in `obj`.
 *
 * @param {Object} obj
 * @return {Array}
 * @api public
 */

exports.keys = Object.keys || function(obj){
  var keys = [];
  for (var key in obj) {
    if (has.call(obj, key)) {
      keys.push(key);
    }
  }
  return keys;
};

/**
 * Return own values in `obj`.
 *
 * @param {Object} obj
 * @return {Array}
 * @api public
 */

exports.values = function(obj){
  var vals = [];
  for (var key in obj) {
    if (has.call(obj, key)) {
      vals.push(obj[key]);
    }
  }
  return vals;
};

/**
 * Merge `b` into `a`.
 *
 * @param {Object} a
 * @param {Object} b
 * @return {Object} a
 * @api public
 */

exports.merge = function(a, b){
  for (var key in b) {
    if (has.call(b, key)) {
      a[key] = b[key];
    }
  }
  return a;
};

/**
 * Return length of `obj`.
 *
 * @param {Object} obj
 * @return {Number}
 * @api public
 */

exports.length = function(obj){
  return exports.keys(obj).length;
};

/**
 * Check if `obj` is empty.
 *
 * @param {Object} obj
 * @return {Boolean}
 * @api public
 */

exports.isEmpty = function(obj){
  return 0 == exports.length(obj);
};
},{}],44:[function(_dereq_,module,exports){
/**
 * Parses an URI
 *
 * @author Steven Levithan <stevenlevithan.com> (MIT license)
 * @api private
 */

var re = /^(?:(?![^:@]+:[^:@\/]*@)(http|https|ws|wss):\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?((?:[a-f0-9]{0,4}:){2,7}[a-f0-9]{0,4}|[^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/;

var parts = [
    'source', 'protocol', 'authority', 'userInfo', 'user', 'password', 'host'
  , 'port', 'relative', 'path', 'directory', 'file', 'query', 'anchor'
];

module.exports = function parseuri(str) {
  var m = re.exec(str || '')
    , uri = {}
    , i = 14;

  while (i--) {
    uri[parts[i]] = m[i] || '';
  }

  return uri;
};

},{}],45:[function(_dereq_,module,exports){
(function (global){
/*global Blob,File*/

/**
 * Module requirements
 */

var isArray = _dereq_('isarray');
var isBuf = _dereq_('./is-buffer');

/**
 * Replaces every Buffer | ArrayBuffer in packet with a numbered placeholder.
 * Anything with blobs or files should be fed through removeBlobs before coming
 * here.
 *
 * @param {Object} packet - socket.io event packet
 * @return {Object} with deconstructed packet and list of buffers
 * @api public
 */

exports.deconstructPacket = function(packet){
  var buffers = [];
  var packetData = packet.data;

  function _deconstructPacket(data) {
    if (!data) return data;

    if (isBuf(data)) {
      var placeholder = { _placeholder: true, num: buffers.length };
      buffers.push(data);
      return placeholder;
    } else if (isArray(data)) {
      var newData = new Array(data.length);
      for (var i = 0; i < data.length; i++) {
        newData[i] = _deconstructPacket(data[i]);
      }
      return newData;
    } else if ('object' == typeof data && !(data instanceof Date)) {
      var newData = {};
      for (var key in data) {
        newData[key] = _deconstructPacket(data[key]);
      }
      return newData;
    }
    return data;
  }

  var pack = packet;
  pack.data = _deconstructPacket(packetData);
  pack.attachments = buffers.length; // number of binary 'attachments'
  return {packet: pack, buffers: buffers};
};

/**
 * Reconstructs a binary packet from its placeholder packet and buffers
 *
 * @param {Object} packet - event packet with placeholders
 * @param {Array} buffers - binary buffers to put in placeholder positions
 * @return {Object} reconstructed packet
 * @api public
 */

exports.reconstructPacket = function(packet, buffers) {
  var curPlaceHolder = 0;

  function _reconstructPacket(data) {
    if (data && data._placeholder) {
      var buf = buffers[data.num]; // appropriate buffer (should be natural order anyway)
      return buf;
    } else if (isArray(data)) {
      for (var i = 0; i < data.length; i++) {
        data[i] = _reconstructPacket(data[i]);
      }
      return data;
    } else if (data && 'object' == typeof data) {
      for (var key in data) {
        data[key] = _reconstructPacket(data[key]);
      }
      return data;
    }
    return data;
  }

  packet.data = _reconstructPacket(packet.data);
  packet.attachments = undefined; // no longer useful
  return packet;
};

/**
 * Asynchronously removes Blobs or Files from data via
 * FileReader's readAsArrayBuffer method. Used before encoding
 * data as msgpack. Calls callback with the blobless data.
 *
 * @param {Object} data
 * @param {Function} callback
 * @api private
 */

exports.removeBlobs = function(data, callback) {
  function _removeBlobs(obj, curKey, containingObject) {
    if (!obj) return obj;

    // convert any blob
    if ((global.Blob && obj instanceof Blob) ||
        (global.File && obj instanceof File)) {
      pendingBlobs++;

      // async filereader
      var fileReader = new FileReader();
      fileReader.onload = function() { // this.result == arraybuffer
        if (containingObject) {
          containingObject[curKey] = this.result;
        }
        else {
          bloblessData = this.result;
        }

        // if nothing pending its callback time
        if(! --pendingBlobs) {
          callback(bloblessData);
        }
      };

      fileReader.readAsArrayBuffer(obj); // blob -> arraybuffer
    } else if (isArray(obj)) { // handle array
      for (var i = 0; i < obj.length; i++) {
        _removeBlobs(obj[i], i, obj);
      }
    } else if (obj && 'object' == typeof obj && !isBuf(obj)) { // and object
      for (var key in obj) {
        _removeBlobs(obj[key], key, obj);
      }
    }
  }

  var pendingBlobs = 0;
  var bloblessData = data;
  _removeBlobs(bloblessData);
  if (!pendingBlobs) {
    callback(bloblessData);
  }
};

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"./is-buffer":47,"isarray":48}],46:[function(_dereq_,module,exports){

/**
 * Module dependencies.
 */

var debug = _dereq_('debug')('socket.io-parser');
var json = _dereq_('json3');
var isArray = _dereq_('isarray');
var Emitter = _dereq_('component-emitter');
var binary = _dereq_('./binary');
var isBuf = _dereq_('./is-buffer');

/**
 * Protocol version.
 *
 * @api public
 */

exports.protocol = 4;

/**
 * Packet types.
 *
 * @api public
 */

exports.types = [
  'CONNECT',
  'DISCONNECT',
  'EVENT',
  'BINARY_EVENT',
  'ACK',
  'BINARY_ACK',
  'ERROR'
];

/**
 * Packet type `connect`.
 *
 * @api public
 */

exports.CONNECT = 0;

/**
 * Packet type `disconnect`.
 *
 * @api public
 */

exports.DISCONNECT = 1;

/**
 * Packet type `event`.
 *
 * @api public
 */

exports.EVENT = 2;

/**
 * Packet type `ack`.
 *
 * @api public
 */

exports.ACK = 3;

/**
 * Packet type `error`.
 *
 * @api public
 */

exports.ERROR = 4;

/**
 * Packet type 'binary event'
 *
 * @api public
 */

exports.BINARY_EVENT = 5;

/**
 * Packet type `binary ack`. For acks with binary arguments.
 *
 * @api public
 */

exports.BINARY_ACK = 6;

/**
 * Encoder constructor.
 *
 * @api public
 */

exports.Encoder = Encoder;

/**
 * Decoder constructor.
 *
 * @api public
 */

exports.Decoder = Decoder;

/**
 * A socket.io Encoder instance
 *
 * @api public
 */

function Encoder() {}

/**
 * Encode a packet as a single string if non-binary, or as a
 * buffer sequence, depending on packet type.
 *
 * @param {Object} obj - packet object
 * @param {Function} callback - function to handle encodings (likely engine.write)
 * @return Calls callback with Array of encodings
 * @api public
 */

Encoder.prototype.encode = function(obj, callback){
  debug('encoding packet %j', obj);

  if (exports.BINARY_EVENT == obj.type || exports.BINARY_ACK == obj.type) {
    encodeAsBinary(obj, callback);
  }
  else {
    var encoding = encodeAsString(obj);
    callback([encoding]);
  }
};

/**
 * Encode packet as string.
 *
 * @param {Object} packet
 * @return {String} encoded
 * @api private
 */

function encodeAsString(obj) {
  var str = '';
  var nsp = false;

  // first is type
  str += obj.type;

  // attachments if we have them
  if (exports.BINARY_EVENT == obj.type || exports.BINARY_ACK == obj.type) {
    str += obj.attachments;
    str += '-';
  }

  // if we have a namespace other than `/`
  // we append it followed by a comma `,`
  if (obj.nsp && '/' != obj.nsp) {
    nsp = true;
    str += obj.nsp;
  }

  // immediately followed by the id
  if (null != obj.id) {
    if (nsp) {
      str += ',';
      nsp = false;
    }
    str += obj.id;
  }

  // json data
  if (null != obj.data) {
    if (nsp) str += ',';
    str += json.stringify(obj.data);
  }

  debug('encoded %j as %s', obj, str);
  return str;
}

/**
 * Encode packet as 'buffer sequence' by removing blobs, and
 * deconstructing packet into object with placeholders and
 * a list of buffers.
 *
 * @param {Object} packet
 * @return {Buffer} encoded
 * @api private
 */

function encodeAsBinary(obj, callback) {

  function writeEncoding(bloblessData) {
    var deconstruction = binary.deconstructPacket(bloblessData);
    var pack = encodeAsString(deconstruction.packet);
    var buffers = deconstruction.buffers;

    buffers.unshift(pack); // add packet info to beginning of data list
    callback(buffers); // write all the buffers
  }

  binary.removeBlobs(obj, writeEncoding);
}

/**
 * A socket.io Decoder instance
 *
 * @return {Object} decoder
 * @api public
 */

function Decoder() {
  this.reconstructor = null;
}

/**
 * Mix in `Emitter` with Decoder.
 */

Emitter(Decoder.prototype);

/**
 * Decodes an ecoded packet string into packet JSON.
 *
 * @param {String} obj - encoded packet
 * @return {Object} packet
 * @api public
 */

Decoder.prototype.add = function(obj) {
  var packet;
  if ('string' == typeof obj) {
    packet = decodeString(obj);
    if (exports.BINARY_EVENT == packet.type || exports.BINARY_ACK == packet.type) { // binary packet's json
      this.reconstructor = new BinaryReconstructor(packet);

      // no attachments, labeled binary but no binary data to follow
      if (this.reconstructor.reconPack.attachments === 0) {
        this.emit('decoded', packet);
      }
    } else { // non-binary full packet
      this.emit('decoded', packet);
    }
  }
  else if (isBuf(obj) || obj.base64) { // raw binary data
    if (!this.reconstructor) {
      throw new Error('got binary data when not reconstructing a packet');
    } else {
      packet = this.reconstructor.takeBinaryData(obj);
      if (packet) { // received final buffer
        this.reconstructor = null;
        this.emit('decoded', packet);
      }
    }
  }
  else {
    throw new Error('Unknown type: ' + obj);
  }
};

/**
 * Decode a packet String (JSON data)
 *
 * @param {String} str
 * @return {Object} packet
 * @api private
 */

function decodeString(str) {
  var p = {};
  var i = 0;

  // look up type
  p.type = Number(str.charAt(0));
  if (null == exports.types[p.type]) return error();

  // look up attachments if type binary
  if (exports.BINARY_EVENT == p.type || exports.BINARY_ACK == p.type) {
    var buf = '';
    while (str.charAt(++i) != '-') {
      buf += str.charAt(i);
      if (i == str.length) break;
    }
    if (buf != Number(buf) || str.charAt(i) != '-') {
      throw new Error('Illegal attachments');
    }
    p.attachments = Number(buf);
  }

  // look up namespace (if any)
  if ('/' == str.charAt(i + 1)) {
    p.nsp = '';
    while (++i) {
      var c = str.charAt(i);
      if (',' == c) break;
      p.nsp += c;
      if (i == str.length) break;
    }
  } else {
    p.nsp = '/';
  }

  // look up id
  var next = str.charAt(i + 1);
  if ('' !== next && Number(next) == next) {
    p.id = '';
    while (++i) {
      var c = str.charAt(i);
      if (null == c || Number(c) != c) {
        --i;
        break;
      }
      p.id += str.charAt(i);
      if (i == str.length) break;
    }
    p.id = Number(p.id);
  }

  // look up json data
  if (str.charAt(++i)) {
    try {
      p.data = json.parse(str.substr(i));
    } catch(e){
      return error();
    }
  }

  debug('decoded %s as %j', str, p);
  return p;
}

/**
 * Deallocates a parser's resources
 *
 * @api public
 */

Decoder.prototype.destroy = function() {
  if (this.reconstructor) {
    this.reconstructor.finishedReconstruction();
  }
};

/**
 * A manager of a binary event's 'buffer sequence'. Should
 * be constructed whenever a packet of type BINARY_EVENT is
 * decoded.
 *
 * @param {Object} packet
 * @return {BinaryReconstructor} initialized reconstructor
 * @api private
 */

function BinaryReconstructor(packet) {
  this.reconPack = packet;
  this.buffers = [];
}

/**
 * Method to be called when binary data received from connection
 * after a BINARY_EVENT packet.
 *
 * @param {Buffer | ArrayBuffer} binData - the raw binary data received
 * @return {null | Object} returns null if more binary data is expected or
 *   a reconstructed packet object if all buffers have been received.
 * @api private
 */

BinaryReconstructor.prototype.takeBinaryData = function(binData) {
  this.buffers.push(binData);
  if (this.buffers.length == this.reconPack.attachments) { // done with buffer list
    var packet = binary.reconstructPacket(this.reconPack, this.buffers);
    this.finishedReconstruction();
    return packet;
  }
  return null;
};

/**
 * Cleans up binary packet reconstruction variables.
 *
 * @api private
 */

BinaryReconstructor.prototype.finishedReconstruction = function() {
  this.reconPack = null;
  this.buffers = [];
};

function error(data){
  return {
    type: exports.ERROR,
    data: 'parser error'
  };
}

},{"./binary":45,"./is-buffer":47,"component-emitter":9,"debug":10,"isarray":48,"json3":49}],47:[function(_dereq_,module,exports){
(function (global){

module.exports = isBuf;

/**
 * Returns true if obj is a buffer or an arraybuffer.
 *
 * @api private
 */

function isBuf(obj) {
  return (global.Buffer && global.Buffer.isBuffer(obj)) ||
         (global.ArrayBuffer && obj instanceof ArrayBuffer);
}

}).call(this,typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],48:[function(_dereq_,module,exports){
module.exports=_dereq_(32)
},{}],49:[function(_dereq_,module,exports){
/*! JSON v3.2.6 | http://bestiejs.github.io/json3 | Copyright 2012-2013, Kit Cambridge | http://kit.mit-license.org */
;(function (window) {
  // Convenience aliases.
  var getClass = {}.toString, isProperty, forEach, undef;

  // Detect the `define` function exposed by asynchronous module loaders. The
  // strict `define` check is necessary for compatibility with `r.js`.
  var isLoader = typeof define === "function" && define.amd;

  // Detect native implementations.
  var nativeJSON = typeof JSON == "object" && JSON;

  // Set up the JSON 3 namespace, preferring the CommonJS `exports` object if
  // available.
  var JSON3 = typeof exports == "object" && exports && !exports.nodeType && exports;

  if (JSON3 && nativeJSON) {
    // Explicitly delegate to the native `stringify` and `parse`
    // implementations in CommonJS environments.
    JSON3.stringify = nativeJSON.stringify;
    JSON3.parse = nativeJSON.parse;
  } else {
    // Export for web browsers, JavaScript engines, and asynchronous module
    // loaders, using the global `JSON` object if available.
    JSON3 = window.JSON = nativeJSON || {};
  }

  // Test the `Date#getUTC*` methods. Based on work by @Yaffle.
  var isExtended = new Date(-3509827334573292);
  try {
    // The `getUTCFullYear`, `Month`, and `Date` methods return nonsensical
    // results for certain dates in Opera >= 10.53.
    isExtended = isExtended.getUTCFullYear() == -109252 && isExtended.getUTCMonth() === 0 && isExtended.getUTCDate() === 1 &&
      // Safari < 2.0.2 stores the internal millisecond time value correctly,
      // but clips the values returned by the date methods to the range of
      // signed 32-bit integers ([-2 ** 31, 2 ** 31 - 1]).
      isExtended.getUTCHours() == 10 && isExtended.getUTCMinutes() == 37 && isExtended.getUTCSeconds() == 6 && isExtended.getUTCMilliseconds() == 708;
  } catch (exception) {}

  // Internal: Determines whether the native `JSON.stringify` and `parse`
  // implementations are spec-compliant. Based on work by Ken Snyder.
  function has(name) {
    if (has[name] !== undef) {
      // Return cached feature test result.
      return has[name];
    }

    var isSupported;
    if (name == "bug-string-char-index") {
      // IE <= 7 doesn't support accessing string characters using square
      // bracket notation. IE 8 only supports this for primitives.
      isSupported = "a"[0] != "a";
    } else if (name == "json") {
      // Indicates whether both `JSON.stringify` and `JSON.parse` are
      // supported.
      isSupported = has("json-stringify") && has("json-parse");
    } else {
      var value, serialized = '{"a":[1,true,false,null,"\\u0000\\b\\n\\f\\r\\t"]}';
      // Test `JSON.stringify`.
      if (name == "json-stringify") {
        var stringify = JSON3.stringify, stringifySupported = typeof stringify == "function" && isExtended;
        if (stringifySupported) {
          // A test function object with a custom `toJSON` method.
          (value = function () {
            return 1;
          }).toJSON = value;
          try {
            stringifySupported =
              // Firefox 3.1b1 and b2 serialize string, number, and boolean
              // primitives as object literals.
              stringify(0) === "0" &&
              // FF 3.1b1, b2, and JSON 2 serialize wrapped primitives as object
              // literals.
              stringify(new Number()) === "0" &&
              stringify(new String()) == '""' &&
              // FF 3.1b1, 2 throw an error if the value is `null`, `undefined`, or
              // does not define a canonical JSON representation (this applies to
              // objects with `toJSON` properties as well, *unless* they are nested
              // within an object or array).
              stringify(getClass) === undef &&
              // IE 8 serializes `undefined` as `"undefined"`. Safari <= 5.1.7 and
              // FF 3.1b3 pass this test.
              stringify(undef) === undef &&
              // Safari <= 5.1.7 and FF 3.1b3 throw `Error`s and `TypeError`s,
              // respectively, if the value is omitted entirely.
              stringify() === undef &&
              // FF 3.1b1, 2 throw an error if the given value is not a number,
              // string, array, object, Boolean, or `null` literal. This applies to
              // objects with custom `toJSON` methods as well, unless they are nested
              // inside object or array literals. YUI 3.0.0b1 ignores custom `toJSON`
              // methods entirely.
              stringify(value) === "1" &&
              stringify([value]) == "[1]" &&
              // Prototype <= 1.6.1 serializes `[undefined]` as `"[]"` instead of
              // `"[null]"`.
              stringify([undef]) == "[null]" &&
              // YUI 3.0.0b1 fails to serialize `null` literals.
              stringify(null) == "null" &&
              // FF 3.1b1, 2 halts serialization if an array contains a function:
              // `[1, true, getClass, 1]` serializes as "[1,true,],". FF 3.1b3
              // elides non-JSON values from objects and arrays, unless they
              // define custom `toJSON` methods.
              stringify([undef, getClass, null]) == "[null,null,null]" &&
              // Simple serialization test. FF 3.1b1 uses Unicode escape sequences
              // where character escape codes are expected (e.g., `\b` => `\u0008`).
              stringify({ "a": [value, true, false, null, "\x00\b\n\f\r\t"] }) == serialized &&
              // FF 3.1b1 and b2 ignore the `filter` and `width` arguments.
              stringify(null, value) === "1" &&
              stringify([1, 2], null, 1) == "[\n 1,\n 2\n]" &&
              // JSON 2, Prototype <= 1.7, and older WebKit builds incorrectly
              // serialize extended years.
              stringify(new Date(-8.64e15)) == '"-271821-04-20T00:00:00.000Z"' &&
              // The milliseconds are optional in ES 5, but required in 5.1.
              stringify(new Date(8.64e15)) == '"+275760-09-13T00:00:00.000Z"' &&
              // Firefox <= 11.0 incorrectly serializes years prior to 0 as negative
              // four-digit years instead of six-digit years. Credits: @Yaffle.
              stringify(new Date(-621987552e5)) == '"-000001-01-01T00:00:00.000Z"' &&
              // Safari <= 5.1.5 and Opera >= 10.53 incorrectly serialize millisecond
              // values less than 1000. Credits: @Yaffle.
              stringify(new Date(-1)) == '"1969-12-31T23:59:59.999Z"';
          } catch (exception) {
            stringifySupported = false;
          }
        }
        isSupported = stringifySupported;
      }
      // Test `JSON.parse`.
      if (name == "json-parse") {
        var parse = JSON3.parse;
        if (typeof parse == "function") {
          try {
            // FF 3.1b1, b2 will throw an exception if a bare literal is provided.
            // Conforming implementations should also coerce the initial argument to
            // a string prior to parsing.
            if (parse("0") === 0 && !parse(false)) {
              // Simple parsing test.
              value = parse(serialized);
              var parseSupported = value["a"].length == 5 && value["a"][0] === 1;
              if (parseSupported) {
                try {
                  // Safari <= 5.1.2 and FF 3.1b1 allow unescaped tabs in strings.
                  parseSupported = !parse('"\t"');
                } catch (exception) {}
                if (parseSupported) {
                  try {
                    // FF 4.0 and 4.0.1 allow leading `+` signs and leading
                    // decimal points. FF 4.0, 4.0.1, and IE 9-10 also allow
                    // certain octal literals.
                    parseSupported = parse("01") !== 1;
                  } catch (exception) {}
                }
                if (parseSupported) {
                  try {
                    // FF 4.0, 4.0.1, and Rhino 1.7R3-R4 allow trailing decimal
                    // points. These environments, along with FF 3.1b1 and 2,
                    // also allow trailing commas in JSON objects and arrays.
                    parseSupported = parse("1.") !== 1;
                  } catch (exception) {}
                }
              }
            }
          } catch (exception) {
            parseSupported = false;
          }
        }
        isSupported = parseSupported;
      }
    }
    return has[name] = !!isSupported;
  }

  if (!has("json")) {
    // Common `[[Class]]` name aliases.
    var functionClass = "[object Function]";
    var dateClass = "[object Date]";
    var numberClass = "[object Number]";
    var stringClass = "[object String]";
    var arrayClass = "[object Array]";
    var booleanClass = "[object Boolean]";

    // Detect incomplete support for accessing string characters by index.
    var charIndexBuggy = has("bug-string-char-index");

    // Define additional utility methods if the `Date` methods are buggy.
    if (!isExtended) {
      var floor = Math.floor;
      // A mapping between the months of the year and the number of days between
      // January 1st and the first of the respective month.
      var Months = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
      // Internal: Calculates the number of days between the Unix epoch and the
      // first day of the given month.
      var getDay = function (year, month) {
        return Months[month] + 365 * (year - 1970) + floor((year - 1969 + (month = +(month > 1))) / 4) - floor((year - 1901 + month) / 100) + floor((year - 1601 + month) / 400);
      };
    }

    // Internal: Determines if a property is a direct property of the given
    // object. Delegates to the native `Object#hasOwnProperty` method.
    if (!(isProperty = {}.hasOwnProperty)) {
      isProperty = function (property) {
        var members = {}, constructor;
        if ((members.__proto__ = null, members.__proto__ = {
          // The *proto* property cannot be set multiple times in recent
          // versions of Firefox and SeaMonkey.
          "toString": 1
        }, members).toString != getClass) {
          // Safari <= 2.0.3 doesn't implement `Object#hasOwnProperty`, but
          // supports the mutable *proto* property.
          isProperty = function (property) {
            // Capture and break the object's prototype chain (see section 8.6.2
            // of the ES 5.1 spec). The parenthesized expression prevents an
            // unsafe transformation by the Closure Compiler.
            var original = this.__proto__, result = property in (this.__proto__ = null, this);
            // Restore the original prototype chain.
            this.__proto__ = original;
            return result;
          };
        } else {
          // Capture a reference to the top-level `Object` constructor.
          constructor = members.constructor;
          // Use the `constructor` property to simulate `Object#hasOwnProperty` in
          // other environments.
          isProperty = function (property) {
            var parent = (this.constructor || constructor).prototype;
            return property in this && !(property in parent && this[property] === parent[property]);
          };
        }
        members = null;
        return isProperty.call(this, property);
      };
    }

    // Internal: A set of primitive types used by `isHostType`.
    var PrimitiveTypes = {
      'boolean': 1,
      'number': 1,
      'string': 1,
      'undefined': 1
    };

    // Internal: Determines if the given object `property` value is a
    // non-primitive.
    var isHostType = function (object, property) {
      var type = typeof object[property];
      return type == 'object' ? !!object[property] : !PrimitiveTypes[type];
    };

    // Internal: Normalizes the `for...in` iteration algorithm across
    // environments. Each enumerated key is yielded to a `callback` function.
    forEach = function (object, callback) {
      var size = 0, Properties, members, property;

      // Tests for bugs in the current environment's `for...in` algorithm. The
      // `valueOf` property inherits the non-enumerable flag from
      // `Object.prototype` in older versions of IE, Netscape, and Mozilla.
      (Properties = function () {
        this.valueOf = 0;
      }).prototype.valueOf = 0;

      // Iterate over a new instance of the `Properties` class.
      members = new Properties();
      for (property in members) {
        // Ignore all properties inherited from `Object.prototype`.
        if (isProperty.call(members, property)) {
          size++;
        }
      }
      Properties = members = null;

      // Normalize the iteration algorithm.
      if (!size) {
        // A list of non-enumerable properties inherited from `Object.prototype`.
        members = ["valueOf", "toString", "toLocaleString", "propertyIsEnumerable", "isPrototypeOf", "hasOwnProperty", "constructor"];
        // IE <= 8, Mozilla 1.0, and Netscape 6.2 ignore shadowed non-enumerable
        // properties.
        forEach = function (object, callback) {
          var isFunction = getClass.call(object) == functionClass, property, length;
          var hasProperty = !isFunction && typeof object.constructor != 'function' && isHostType(object, 'hasOwnProperty') ? object.hasOwnProperty : isProperty;
          for (property in object) {
            // Gecko <= 1.0 enumerates the `prototype` property of functions under
            // certain conditions; IE does not.
            if (!(isFunction && property == "prototype") && hasProperty.call(object, property)) {
              callback(property);
            }
          }
          // Manually invoke the callback for each non-enumerable property.
          for (length = members.length; property = members[--length]; hasProperty.call(object, property) && callback(property));
        };
      } else if (size == 2) {
        // Safari <= 2.0.4 enumerates shadowed properties twice.
        forEach = function (object, callback) {
          // Create a set of iterated properties.
          var members = {}, isFunction = getClass.call(object) == functionClass, property;
          for (property in object) {
            // Store each property name to prevent double enumeration. The
            // `prototype` property of functions is not enumerated due to cross-
            // environment inconsistencies.
            if (!(isFunction && property == "prototype") && !isProperty.call(members, property) && (members[property] = 1) && isProperty.call(object, property)) {
              callback(property);
            }
          }
        };
      } else {
        // No bugs detected; use the standard `for...in` algorithm.
        forEach = function (object, callback) {
          var isFunction = getClass.call(object) == functionClass, property, isConstructor;
          for (property in object) {
            if (!(isFunction && property == "prototype") && isProperty.call(object, property) && !(isConstructor = property === "constructor")) {
              callback(property);
            }
          }
          // Manually invoke the callback for the `constructor` property due to
          // cross-environment inconsistencies.
          if (isConstructor || isProperty.call(object, (property = "constructor"))) {
            callback(property);
          }
        };
      }
      return forEach(object, callback);
    };

    // Public: Serializes a JavaScript `value` as a JSON string. The optional
    // `filter` argument may specify either a function that alters how object and
    // array members are serialized, or an array of strings and numbers that
    // indicates which properties should be serialized. The optional `width`
    // argument may be either a string or number that specifies the indentation
    // level of the output.
    if (!has("json-stringify")) {
      // Internal: A map of control characters and their escaped equivalents.
      var Escapes = {
        92: "\\\\",
        34: '\\"',
        8: "\\b",
        12: "\\f",
        10: "\\n",
        13: "\\r",
        9: "\\t"
      };

      // Internal: Converts `value` into a zero-padded string such that its
      // length is at least equal to `width`. The `width` must be <= 6.
      var leadingZeroes = "000000";
      var toPaddedString = function (width, value) {
        // The `|| 0` expression is necessary to work around a bug in
        // Opera <= 7.54u2 where `0 == -0`, but `String(-0) !== "0"`.
        return (leadingZeroes + (value || 0)).slice(-width);
      };

      // Internal: Double-quotes a string `value`, replacing all ASCII control
      // characters (characters with code unit values between 0 and 31) with
      // their escaped equivalents. This is an implementation of the
      // `Quote(value)` operation defined in ES 5.1 section 15.12.3.
      var unicodePrefix = "\\u00";
      var quote = function (value) {
        var result = '"', index = 0, length = value.length, isLarge = length > 10 && charIndexBuggy, symbols;
        if (isLarge) {
          symbols = value.split("");
        }
        for (; index < length; index++) {
          var charCode = value.charCodeAt(index);
          // If the character is a control character, append its Unicode or
          // shorthand escape sequence; otherwise, append the character as-is.
          switch (charCode) {
            case 8: case 9: case 10: case 12: case 13: case 34: case 92:
              result += Escapes[charCode];
              break;
            default:
              if (charCode < 32) {
                result += unicodePrefix + toPaddedString(2, charCode.toString(16));
                break;
              }
              result += isLarge ? symbols[index] : charIndexBuggy ? value.charAt(index) : value[index];
          }
        }
        return result + '"';
      };

      // Internal: Recursively serializes an object. Implements the
      // `Str(key, holder)`, `JO(value)`, and `JA(value)` operations.
      var serialize = function (property, object, callback, properties, whitespace, indentation, stack) {
        var value, className, year, month, date, time, hours, minutes, seconds, milliseconds, results, element, index, length, prefix, result;
        try {
          // Necessary for host object support.
          value = object[property];
        } catch (exception) {}
        if (typeof value == "object" && value) {
          className = getClass.call(value);
          if (className == dateClass && !isProperty.call(value, "toJSON")) {
            if (value > -1 / 0 && value < 1 / 0) {
              // Dates are serialized according to the `Date#toJSON` method
              // specified in ES 5.1 section 15.9.5.44. See section 15.9.1.15
              // for the ISO 8601 date time string format.
              if (getDay) {
                // Manually compute the year, month, date, hours, minutes,
                // seconds, and milliseconds if the `getUTC*` methods are
                // buggy. Adapted from @Yaffle's `date-shim` project.
                date = floor(value / 864e5);
                for (year = floor(date / 365.2425) + 1970 - 1; getDay(year + 1, 0) <= date; year++);
                for (month = floor((date - getDay(year, 0)) / 30.42); getDay(year, month + 1) <= date; month++);
                date = 1 + date - getDay(year, month);
                // The `time` value specifies the time within the day (see ES
                // 5.1 section 15.9.1.2). The formula `(A % B + B) % B` is used
                // to compute `A modulo B`, as the `%` operator does not
                // correspond to the `modulo` operation for negative numbers.
                time = (value % 864e5 + 864e5) % 864e5;
                // The hours, minutes, seconds, and milliseconds are obtained by
                // decomposing the time within the day. See section 15.9.1.10.
                hours = floor(time / 36e5) % 24;
                minutes = floor(time / 6e4) % 60;
                seconds = floor(time / 1e3) % 60;
                milliseconds = time % 1e3;
              } else {
                year = value.getUTCFullYear();
                month = value.getUTCMonth();
                date = value.getUTCDate();
                hours = value.getUTCHours();
                minutes = value.getUTCMinutes();
                seconds = value.getUTCSeconds();
                milliseconds = value.getUTCMilliseconds();
              }
              // Serialize extended years correctly.
              value = (year <= 0 || year >= 1e4 ? (year < 0 ? "-" : "+") + toPaddedString(6, year < 0 ? -year : year) : toPaddedString(4, year)) +
                "-" + toPaddedString(2, month + 1) + "-" + toPaddedString(2, date) +
                // Months, dates, hours, minutes, and seconds should have two
                // digits; milliseconds should have three.
                "T" + toPaddedString(2, hours) + ":" + toPaddedString(2, minutes) + ":" + toPaddedString(2, seconds) +
                // Milliseconds are optional in ES 5.0, but required in 5.1.
                "." + toPaddedString(3, milliseconds) + "Z";
            } else {
              value = null;
            }
          } else if (typeof value.toJSON == "function" && ((className != numberClass && className != stringClass && className != arrayClass) || isProperty.call(value, "toJSON"))) {
            // Prototype <= 1.6.1 adds non-standard `toJSON` methods to the
            // `Number`, `String`, `Date`, and `Array` prototypes. JSON 3
            // ignores all `toJSON` methods on these objects unless they are
            // defined directly on an instance.
            value = value.toJSON(property);
          }
        }
        if (callback) {
          // If a replacement function was provided, call it to obtain the value
          // for serialization.
          value = callback.call(object, property, value);
        }
        if (value === null) {
          return "null";
        }
        className = getClass.call(value);
        if (className == booleanClass) {
          // Booleans are represented literally.
          return "" + value;
        } else if (className == numberClass) {
          // JSON numbers must be finite. `Infinity` and `NaN` are serialized as
          // `"null"`.
          return value > -1 / 0 && value < 1 / 0 ? "" + value : "null";
        } else if (className == stringClass) {
          // Strings are double-quoted and escaped.
          return quote("" + value);
        }
        // Recursively serialize objects and arrays.
        if (typeof value == "object") {
          // Check for cyclic structures. This is a linear search; performance
          // is inversely proportional to the number of unique nested objects.
          for (length = stack.length; length--;) {
            if (stack[length] === value) {
              // Cyclic structures cannot be serialized by `JSON.stringify`.
              throw TypeError();
            }
          }
          // Add the object to the stack of traversed objects.
          stack.push(value);
          results = [];
          // Save the current indentation level and indent one additional level.
          prefix = indentation;
          indentation += whitespace;
          if (className == arrayClass) {
            // Recursively serialize array elements.
            for (index = 0, length = value.length; index < length; index++) {
              element = serialize(index, value, callback, properties, whitespace, indentation, stack);
              results.push(element === undef ? "null" : element);
            }
            result = results.length ? (whitespace ? "[\n" + indentation + results.join(",\n" + indentation) + "\n" + prefix + "]" : ("[" + results.join(",") + "]")) : "[]";
          } else {
            // Recursively serialize object members. Members are selected from
            // either a user-specified list of property names, or the object
            // itself.
            forEach(properties || value, function (property) {
              var element = serialize(property, value, callback, properties, whitespace, indentation, stack);
              if (element !== undef) {
                // According to ES 5.1 section 15.12.3: "If `gap` {whitespace}
                // is not the empty string, let `member` {quote(property) + ":"}
                // be the concatenation of `member` and the `space` character."
                // The "`space` character" refers to the literal space
                // character, not the `space` {width} argument provided to
                // `JSON.stringify`.
                results.push(quote(property) + ":" + (whitespace ? " " : "") + element);
              }
            });
            result = results.length ? (whitespace ? "{\n" + indentation + results.join(",\n" + indentation) + "\n" + prefix + "}" : ("{" + results.join(",") + "}")) : "{}";
          }
          // Remove the object from the traversed object stack.
          stack.pop();
          return result;
        }
      };

      // Public: `JSON.stringify`. See ES 5.1 section 15.12.3.
      JSON3.stringify = function (source, filter, width) {
        var whitespace, callback, properties, className;
        if (typeof filter == "function" || typeof filter == "object" && filter) {
          if ((className = getClass.call(filter)) == functionClass) {
            callback = filter;
          } else if (className == arrayClass) {
            // Convert the property names array into a makeshift set.
            properties = {};
            for (var index = 0, length = filter.length, value; index < length; value = filter[index++], ((className = getClass.call(value)), className == stringClass || className == numberClass) && (properties[value] = 1));
          }
        }
        if (width) {
          if ((className = getClass.call(width)) == numberClass) {
            // Convert the `width` to an integer and create a string containing
            // `width` number of space characters.
            if ((width -= width % 1) > 0) {
              for (whitespace = "", width > 10 && (width = 10); whitespace.length < width; whitespace += " ");
            }
          } else if (className == stringClass) {
            whitespace = width.length <= 10 ? width : width.slice(0, 10);
          }
        }
        // Opera <= 7.54u2 discards the values associated with empty string keys
        // (`""`) only if they are used directly within an object member list
        // (e.g., `!("" in { "": 1})`).
        return serialize("", (value = {}, value[""] = source, value), callback, properties, whitespace, "", []);
      };
    }

    // Public: Parses a JSON source string.
    if (!has("json-parse")) {
      var fromCharCode = String.fromCharCode;

      // Internal: A map of escaped control characters and their unescaped
      // equivalents.
      var Unescapes = {
        92: "\\",
        34: '"',
        47: "/",
        98: "\b",
        116: "\t",
        110: "\n",
        102: "\f",
        114: "\r"
      };

      // Internal: Stores the parser state.
      var Index, Source;

      // Internal: Resets the parser state and throws a `SyntaxError`.
      var abort = function() {
        Index = Source = null;
        throw SyntaxError();
      };

      // Internal: Returns the next token, or `"$"` if the parser has reached
      // the end of the source string. A token may be a string, number, `null`
      // literal, or Boolean literal.
      var lex = function () {
        var source = Source, length = source.length, value, begin, position, isSigned, charCode;
        while (Index < length) {
          charCode = source.charCodeAt(Index);
          switch (charCode) {
            case 9: case 10: case 13: case 32:
              // Skip whitespace tokens, including tabs, carriage returns, line
              // feeds, and space characters.
              Index++;
              break;
            case 123: case 125: case 91: case 93: case 58: case 44:
              // Parse a punctuator token (`{`, `}`, `[`, `]`, `:`, or `,`) at
              // the current position.
              value = charIndexBuggy ? source.charAt(Index) : source[Index];
              Index++;
              return value;
            case 34:
              // `"` delimits a JSON string; advance to the next character and
              // begin parsing the string. String tokens are prefixed with the
              // sentinel `@` character to distinguish them from punctuators and
              // end-of-string tokens.
              for (value = "@", Index++; Index < length;) {
                charCode = source.charCodeAt(Index);
                if (charCode < 32) {
                  // Unescaped ASCII control characters (those with a code unit
                  // less than the space character) are not permitted.
                  abort();
                } else if (charCode == 92) {
                  // A reverse solidus (`\`) marks the beginning of an escaped
                  // control character (including `"`, `\`, and `/`) or Unicode
                  // escape sequence.
                  charCode = source.charCodeAt(++Index);
                  switch (charCode) {
                    case 92: case 34: case 47: case 98: case 116: case 110: case 102: case 114:
                      // Revive escaped control characters.
                      value += Unescapes[charCode];
                      Index++;
                      break;
                    case 117:
                      // `\u` marks the beginning of a Unicode escape sequence.
                      // Advance to the first character and validate the
                      // four-digit code point.
                      begin = ++Index;
                      for (position = Index + 4; Index < position; Index++) {
                        charCode = source.charCodeAt(Index);
                        // A valid sequence comprises four hexdigits (case-
                        // insensitive) that form a single hexadecimal value.
                        if (!(charCode >= 48 && charCode <= 57 || charCode >= 97 && charCode <= 102 || charCode >= 65 && charCode <= 70)) {
                          // Invalid Unicode escape sequence.
                          abort();
                        }
                      }
                      // Revive the escaped character.
                      value += fromCharCode("0x" + source.slice(begin, Index));
                      break;
                    default:
                      // Invalid escape sequence.
                      abort();
                  }
                } else {
                  if (charCode == 34) {
                    // An unescaped double-quote character marks the end of the
                    // string.
                    break;
                  }
                  charCode = source.charCodeAt(Index);
                  begin = Index;
                  // Optimize for the common case where a string is valid.
                  while (charCode >= 32 && charCode != 92 && charCode != 34) {
                    charCode = source.charCodeAt(++Index);
                  }
                  // Append the string as-is.
                  value += source.slice(begin, Index);
                }
              }
              if (source.charCodeAt(Index) == 34) {
                // Advance to the next character and return the revived string.
                Index++;
                return value;
              }
              // Unterminated string.
              abort();
            default:
              // Parse numbers and literals.
              begin = Index;
              // Advance past the negative sign, if one is specified.
              if (charCode == 45) {
                isSigned = true;
                charCode = source.charCodeAt(++Index);
              }
              // Parse an integer or floating-point value.
              if (charCode >= 48 && charCode <= 57) {
                // Leading zeroes are interpreted as octal literals.
                if (charCode == 48 && ((charCode = source.charCodeAt(Index + 1)), charCode >= 48 && charCode <= 57)) {
                  // Illegal octal literal.
                  abort();
                }
                isSigned = false;
                // Parse the integer component.
                for (; Index < length && ((charCode = source.charCodeAt(Index)), charCode >= 48 && charCode <= 57); Index++);
                // Floats cannot contain a leading decimal point; however, this
                // case is already accounted for by the parser.
                if (source.charCodeAt(Index) == 46) {
                  position = ++Index;
                  // Parse the decimal component.
                  for (; position < length && ((charCode = source.charCodeAt(position)), charCode >= 48 && charCode <= 57); position++);
                  if (position == Index) {
                    // Illegal trailing decimal.
                    abort();
                  }
                  Index = position;
                }
                // Parse exponents. The `e` denoting the exponent is
                // case-insensitive.
                charCode = source.charCodeAt(Index);
                if (charCode == 101 || charCode == 69) {
                  charCode = source.charCodeAt(++Index);
                  // Skip past the sign following the exponent, if one is
                  // specified.
                  if (charCode == 43 || charCode == 45) {
                    Index++;
                  }
                  // Parse the exponential component.
                  for (position = Index; position < length && ((charCode = source.charCodeAt(position)), charCode >= 48 && charCode <= 57); position++);
                  if (position == Index) {
                    // Illegal empty exponent.
                    abort();
                  }
                  Index = position;
                }
                // Coerce the parsed value to a JavaScript number.
                return +source.slice(begin, Index);
              }
              // A negative sign may only precede numbers.
              if (isSigned) {
                abort();
              }
              // `true`, `false`, and `null` literals.
              if (source.slice(Index, Index + 4) == "true") {
                Index += 4;
                return true;
              } else if (source.slice(Index, Index + 5) == "false") {
                Index += 5;
                return false;
              } else if (source.slice(Index, Index + 4) == "null") {
                Index += 4;
                return null;
              }
              // Unrecognized token.
              abort();
          }
        }
        // Return the sentinel `$` character if the parser has reached the end
        // of the source string.
        return "$";
      };

      // Internal: Parses a JSON `value` token.
      var get = function (value) {
        var results, hasMembers;
        if (value == "$") {
          // Unexpected end of input.
          abort();
        }
        if (typeof value == "string") {
          if ((charIndexBuggy ? value.charAt(0) : value[0]) == "@") {
            // Remove the sentinel `@` character.
            return value.slice(1);
          }
          // Parse object and array literals.
          if (value == "[") {
            // Parses a JSON array, returning a new JavaScript array.
            results = [];
            for (;; hasMembers || (hasMembers = true)) {
              value = lex();
              // A closing square bracket marks the end of the array literal.
              if (value == "]") {
                break;
              }
              // If the array literal contains elements, the current token
              // should be a comma separating the previous element from the
              // next.
              if (hasMembers) {
                if (value == ",") {
                  value = lex();
                  if (value == "]") {
                    // Unexpected trailing `,` in array literal.
                    abort();
                  }
                } else {
                  // A `,` must separate each array element.
                  abort();
                }
              }
              // Elisions and leading commas are not permitted.
              if (value == ",") {
                abort();
              }
              results.push(get(value));
            }
            return results;
          } else if (value == "{") {
            // Parses a JSON object, returning a new JavaScript object.
            results = {};
            for (;; hasMembers || (hasMembers = true)) {
              value = lex();
              // A closing curly brace marks the end of the object literal.
              if (value == "}") {
                break;
              }
              // If the object literal contains members, the current token
              // should be a comma separator.
              if (hasMembers) {
                if (value == ",") {
                  value = lex();
                  if (value == "}") {
                    // Unexpected trailing `,` in object literal.
                    abort();
                  }
                } else {
                  // A `,` must separate each object member.
                  abort();
                }
              }
              // Leading commas are not permitted, object property names must be
              // double-quoted strings, and a `:` must separate each property
              // name and value.
              if (value == "," || typeof value != "string" || (charIndexBuggy ? value.charAt(0) : value[0]) != "@" || lex() != ":") {
                abort();
              }
              results[value.slice(1)] = get(lex());
            }
            return results;
          }
          // Unexpected token encountered.
          abort();
        }
        return value;
      };

      // Internal: Updates a traversed object member.
      var update = function(source, property, callback) {
        var element = walk(source, property, callback);
        if (element === undef) {
          delete source[property];
        } else {
          source[property] = element;
        }
      };

      // Internal: Recursively traverses a parsed JSON object, invoking the
      // `callback` function for each value. This is an implementation of the
      // `Walk(holder, name)` operation defined in ES 5.1 section 15.12.2.
      var walk = function (source, property, callback) {
        var value = source[property], length;
        if (typeof value == "object" && value) {
          // `forEach` can't be used to traverse an array in Opera <= 8.54
          // because its `Object#hasOwnProperty` implementation returns `false`
          // for array indices (e.g., `![1, 2, 3].hasOwnProperty("0")`).
          if (getClass.call(value) == arrayClass) {
            for (length = value.length; length--;) {
              update(value, length, callback);
            }
          } else {
            forEach(value, function (property) {
              update(value, property, callback);
            });
          }
        }
        return callback.call(source, property, value);
      };

      // Public: `JSON.parse`. See ES 5.1 section 15.12.2.
      JSON3.parse = function (source, callback) {
        var result, value;
        Index = 0;
        Source = "" + source;
        result = get(lex());
        // If a JSON string contains multiple tokens, it is invalid.
        if (lex() != "$") {
          abort();
        }
        // Reset the parser state.
        Index = Source = null;
        return callback && getClass.call(callback) == functionClass ? walk((value = {}, value[""] = result, value), "", callback) : result;
      };
    }
  }

  // Export for asynchronous module loaders.
  if (isLoader) {
    define(function () {
      return JSON3;
    });
  }
}(this));

},{}],50:[function(_dereq_,module,exports){
module.exports = toArray

function toArray(list, index) {
    var array = []

    index = index || 0

    for (var i = index || 0; i < list.length; i++) {
        array[i - index] = list[i]
    }

    return array
}

},{}]},{},[1])
(1)
});
;(function (root, factory) {
	if (typeof exports === "object") {
		// CommonJS
		module.exports = exports = factory();
	}
	else if (typeof define === "function" && define.amd) {
		// AMD
		define([], factory);
	}
	else {
		// Global (browser)
		root.CryptoJS = factory();
	}
}(this, function () {

	/**
	 * CryptoJS core components.
	 */
	var CryptoJS = CryptoJS || (function (Math, undefined) {
	    /**
	     * CryptoJS namespace.
	     */
	    var C = {};

	    /**
	     * Library namespace.
	     */
	    var C_lib = C.lib = {};

	    /**
	     * Base object for prototypal inheritance.
	     */
	    var Base = C_lib.Base = (function () {
	        function F() {}

	        return {
	            /**
	             * Creates a new object that inherits from this object.
	             *
	             * @param {Object} overrides Properties to copy into the new object.
	             *
	             * @return {Object} The new object.
	             *
	             * @static
	             *
	             * @example
	             *
	             *     var MyType = CryptoJS.lib.Base.extend({
	             *         field: 'value',
	             *
	             *         method: function () {
	             *         }
	             *     });
	             */
	            extend: function (overrides) {
	                // Spawn
	                F.prototype = this;
	                var subtype = new F();

	                // Augment
	                if (overrides) {
	                    subtype.mixIn(overrides);
	                }

	                // Create default initializer
	                if (!subtype.hasOwnProperty('init')) {
	                    subtype.init = function () {
	                        subtype.$super.init.apply(this, arguments);
	                    };
	                }

	                // Initializer's prototype is the subtype object
	                subtype.init.prototype = subtype;

	                // Reference supertype
	                subtype.$super = this;

	                return subtype;
	            },

	            /**
	             * Extends this object and runs the init method.
	             * Arguments to create() will be passed to init().
	             *
	             * @return {Object} The new object.
	             *
	             * @static
	             *
	             * @example
	             *
	             *     var instance = MyType.create();
	             */
	            create: function () {
	                var instance = this.extend();
	                instance.init.apply(instance, arguments);

	                return instance;
	            },

	            /**
	             * Initializes a newly created object.
	             * Override this method to add some logic when your objects are created.
	             *
	             * @example
	             *
	             *     var MyType = CryptoJS.lib.Base.extend({
	             *         init: function () {
	             *             // ...
	             *         }
	             *     });
	             */
	            init: function () {
	            },

	            /**
	             * Copies properties into this object.
	             *
	             * @param {Object} properties The properties to mix in.
	             *
	             * @example
	             *
	             *     MyType.mixIn({
	             *         field: 'value'
	             *     });
	             */
	            mixIn: function (properties) {
	                for (var propertyName in properties) {
	                    if (properties.hasOwnProperty(propertyName)) {
	                        this[propertyName] = properties[propertyName];
	                    }
	                }

	                // IE won't copy toString using the loop above
	                if (properties.hasOwnProperty('toString')) {
	                    this.toString = properties.toString;
	                }
	            },

	            /**
	             * Creates a copy of this object.
	             *
	             * @return {Object} The clone.
	             *
	             * @example
	             *
	             *     var clone = instance.clone();
	             */
	            clone: function () {
	                return this.init.prototype.extend(this);
	            }
	        };
	    }());

	    /**
	     * An array of 32-bit words.
	     *
	     * @property {Array} words The array of 32-bit words.
	     * @property {number} sigBytes The number of significant bytes in this word array.
	     */
	    var WordArray = C_lib.WordArray = Base.extend({
	        /**
	         * Initializes a newly created word array.
	         *
	         * @param {Array} words (Optional) An array of 32-bit words.
	         * @param {number} sigBytes (Optional) The number of significant bytes in the words.
	         *
	         * @example
	         *
	         *     var wordArray = CryptoJS.lib.WordArray.create();
	         *     var wordArray = CryptoJS.lib.WordArray.create([0x00010203, 0x04050607]);
	         *     var wordArray = CryptoJS.lib.WordArray.create([0x00010203, 0x04050607], 6);
	         */
	        init: function (words, sigBytes) {
	            words = this.words = words || [];

	            if (sigBytes != undefined) {
	                this.sigBytes = sigBytes;
	            } else {
	                this.sigBytes = words.length * 4;
	            }
	        },

	        /**
	         * Converts this word array to a string.
	         *
	         * @param {Encoder} encoder (Optional) The encoding strategy to use. Default: CryptoJS.enc.Hex
	         *
	         * @return {string} The stringified word array.
	         *
	         * @example
	         *
	         *     var string = wordArray + '';
	         *     var string = wordArray.toString();
	         *     var string = wordArray.toString(CryptoJS.enc.Utf8);
	         */
	        toString: function (encoder) {
	            return (encoder || Hex).stringify(this);
	        },

	        /**
	         * Concatenates a word array to this word array.
	         *
	         * @param {WordArray} wordArray The word array to append.
	         *
	         * @return {WordArray} This word array.
	         *
	         * @example
	         *
	         *     wordArray1.concat(wordArray2);
	         */
	        concat: function (wordArray) {
	            // Shortcuts
	            var thisWords = this.words;
	            var thatWords = wordArray.words;
	            var thisSigBytes = this.sigBytes;
	            var thatSigBytes = wordArray.sigBytes;

	            // Clamp excess bits
	            this.clamp();

	            // Concat
	            if (thisSigBytes % 4) {
	                // Copy one byte at a time
	                for (var i = 0; i < thatSigBytes; i++) {
	                    var thatByte = (thatWords[i >>> 2] >>> (24 - (i % 4) * 8)) & 0xff;
	                    thisWords[(thisSigBytes + i) >>> 2] |= thatByte << (24 - ((thisSigBytes + i) % 4) * 8);
	                }
	            } else if (thatWords.length > 0xffff) {
	                // Copy one word at a time
	                for (var i = 0; i < thatSigBytes; i += 4) {
	                    thisWords[(thisSigBytes + i) >>> 2] = thatWords[i >>> 2];
	                }
	            } else {
	                // Copy all words at once
	                thisWords.push.apply(thisWords, thatWords);
	            }
	            this.sigBytes += thatSigBytes;

	            // Chainable
	            return this;
	        },

	        /**
	         * Removes insignificant bits.
	         *
	         * @example
	         *
	         *     wordArray.clamp();
	         */
	        clamp: function () {
	            // Shortcuts
	            var words = this.words;
	            var sigBytes = this.sigBytes;

	            // Clamp
	            words[sigBytes >>> 2] &= 0xffffffff << (32 - (sigBytes % 4) * 8);
	            words.length = Math.ceil(sigBytes / 4);
	        },

	        /**
	         * Creates a copy of this word array.
	         *
	         * @return {WordArray} The clone.
	         *
	         * @example
	         *
	         *     var clone = wordArray.clone();
	         */
	        clone: function () {
	            var clone = Base.clone.call(this);
	            clone.words = this.words.slice(0);

	            return clone;
	        },

	        /**
	         * Creates a word array filled with random bytes.
	         *
	         * @param {number} nBytes The number of random bytes to generate.
	         *
	         * @return {WordArray} The random word array.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var wordArray = CryptoJS.lib.WordArray.random(16);
	         */
	        random: function (nBytes) {
	            var words = [];

	            var r = (function (m_w) {
	                var m_w = m_w;
	                var m_z = 0x3ade68b1;
	                var mask = 0xffffffff;

	                return function () {
	                    m_z = (0x9069 * (m_z & 0xFFFF) + (m_z >> 0x10)) & mask;
	                    m_w = (0x4650 * (m_w & 0xFFFF) + (m_w >> 0x10)) & mask;
	                    var result = ((m_z << 0x10) + m_w) & mask;
	                    result /= 0x100000000;
	                    result += 0.5;
	                    return result * (Math.random() > .5 ? 1 : -1);
	                }
	            });

	            for (var i = 0, rcache; i < nBytes; i += 4) {
	                var _r = r((rcache || Math.random()) * 0x100000000);

	                rcache = _r() * 0x3ade67b7;
	                words.push((_r() * 0x100000000) | 0);
	            }

	            return new WordArray.init(words, nBytes);
	        }
	    });

	    /**
	     * Encoder namespace.
	     */
	    var C_enc = C.enc = {};

	    /**
	     * Hex encoding strategy.
	     */
	    var Hex = C_enc.Hex = {
	        /**
	         * Converts a word array to a hex string.
	         *
	         * @param {WordArray} wordArray The word array.
	         *
	         * @return {string} The hex string.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var hexString = CryptoJS.enc.Hex.stringify(wordArray);
	         */
	        stringify: function (wordArray) {
	            // Shortcuts
	            var words = wordArray.words;
	            var sigBytes = wordArray.sigBytes;

	            // Convert
	            var hexChars = [];
	            for (var i = 0; i < sigBytes; i++) {
	                var bite = (words[i >>> 2] >>> (24 - (i % 4) * 8)) & 0xff;
	                hexChars.push((bite >>> 4).toString(16));
	                hexChars.push((bite & 0x0f).toString(16));
	            }

	            return hexChars.join('');
	        },

	        /**
	         * Converts a hex string to a word array.
	         *
	         * @param {string} hexStr The hex string.
	         *
	         * @return {WordArray} The word array.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var wordArray = CryptoJS.enc.Hex.parse(hexString);
	         */
	        parse: function (hexStr) {
	            // Shortcut
	            var hexStrLength = hexStr.length;

	            // Convert
	            var words = [];
	            for (var i = 0; i < hexStrLength; i += 2) {
	                words[i >>> 3] |= parseInt(hexStr.substr(i, 2), 16) << (24 - (i % 8) * 4);
	            }

	            return new WordArray.init(words, hexStrLength / 2);
	        }
	    };

	    /**
	     * Latin1 encoding strategy.
	     */
	    var Latin1 = C_enc.Latin1 = {
	        /**
	         * Converts a word array to a Latin1 string.
	         *
	         * @param {WordArray} wordArray The word array.
	         *
	         * @return {string} The Latin1 string.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var latin1String = CryptoJS.enc.Latin1.stringify(wordArray);
	         */
	        stringify: function (wordArray) {
	            // Shortcuts
	            var words = wordArray.words;
	            var sigBytes = wordArray.sigBytes;

	            // Convert
	            var latin1Chars = [];
	            for (var i = 0; i < sigBytes; i++) {
	                var bite = (words[i >>> 2] >>> (24 - (i % 4) * 8)) & 0xff;
	                latin1Chars.push(String.fromCharCode(bite));
	            }

	            return latin1Chars.join('');
	        },

	        /**
	         * Converts a Latin1 string to a word array.
	         *
	         * @param {string} latin1Str The Latin1 string.
	         *
	         * @return {WordArray} The word array.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var wordArray = CryptoJS.enc.Latin1.parse(latin1String);
	         */
	        parse: function (latin1Str) {
	            // Shortcut
	            var latin1StrLength = latin1Str.length;

	            // Convert
	            var words = [];
	            for (var i = 0; i < latin1StrLength; i++) {
	                words[i >>> 2] |= (latin1Str.charCodeAt(i) & 0xff) << (24 - (i % 4) * 8);
	            }

	            return new WordArray.init(words, latin1StrLength);
	        }
	    };

	    /**
	     * UTF-8 encoding strategy.
	     */
	    var Utf8 = C_enc.Utf8 = {
	        /**
	         * Converts a word array to a UTF-8 string.
	         *
	         * @param {WordArray} wordArray The word array.
	         *
	         * @return {string} The UTF-8 string.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var utf8String = CryptoJS.enc.Utf8.stringify(wordArray);
	         */
	        stringify: function (wordArray) {
	            try {
	                return decodeURIComponent(escape(Latin1.stringify(wordArray)));
	            } catch (e) {
	                throw new Error('Malformed UTF-8 data');
	            }
	        },

	        /**
	         * Converts a UTF-8 string to a word array.
	         *
	         * @param {string} utf8Str The UTF-8 string.
	         *
	         * @return {WordArray} The word array.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var wordArray = CryptoJS.enc.Utf8.parse(utf8String);
	         */
	        parse: function (utf8Str) {
	            return Latin1.parse(unescape(encodeURIComponent(utf8Str)));
	        }
	    };

	    /**
	     * Abstract buffered block algorithm template.
	     *
	     * The property blockSize must be implemented in a concrete subtype.
	     *
	     * @property {number} _minBufferSize The number of blocks that should be kept unprocessed in the buffer. Default: 0
	     */
	    var BufferedBlockAlgorithm = C_lib.BufferedBlockAlgorithm = Base.extend({
	        /**
	         * Resets this block algorithm's data buffer to its initial state.
	         *
	         * @example
	         *
	         *     bufferedBlockAlgorithm.reset();
	         */
	        reset: function () {
	            // Initial values
	            this._data = new WordArray.init();
	            this._nDataBytes = 0;
	        },

	        /**
	         * Adds new data to this block algorithm's buffer.
	         *
	         * @param {WordArray|string} data The data to append. Strings are converted to a WordArray using UTF-8.
	         *
	         * @example
	         *
	         *     bufferedBlockAlgorithm._append('data');
	         *     bufferedBlockAlgorithm._append(wordArray);
	         */
	        _append: function (data) {
	            // Convert string to WordArray, else assume WordArray already
	            if (typeof data == 'string') {
	                data = Utf8.parse(data);
	            }

	            // Append
	            this._data.concat(data);
	            this._nDataBytes += data.sigBytes;
	        },

	        /**
	         * Processes available data blocks.
	         *
	         * This method invokes _doProcessBlock(offset), which must be implemented by a concrete subtype.
	         *
	         * @param {boolean} doFlush Whether all blocks and partial blocks should be processed.
	         *
	         * @return {WordArray} The processed data.
	         *
	         * @example
	         *
	         *     var processedData = bufferedBlockAlgorithm._process();
	         *     var processedData = bufferedBlockAlgorithm._process(!!'flush');
	         */
	        _process: function (doFlush) {
	            // Shortcuts
	            var data = this._data;
	            var dataWords = data.words;
	            var dataSigBytes = data.sigBytes;
	            var blockSize = this.blockSize;
	            var blockSizeBytes = blockSize * 4;

	            // Count blocks ready
	            var nBlocksReady = dataSigBytes / blockSizeBytes;
	            if (doFlush) {
	                // Round up to include partial blocks
	                nBlocksReady = Math.ceil(nBlocksReady);
	            } else {
	                // Round down to include only full blocks,
	                // less the number of blocks that must remain in the buffer
	                nBlocksReady = Math.max((nBlocksReady | 0) - this._minBufferSize, 0);
	            }

	            // Count words ready
	            var nWordsReady = nBlocksReady * blockSize;

	            // Count bytes ready
	            var nBytesReady = Math.min(nWordsReady * 4, dataSigBytes);

	            // Process blocks
	            if (nWordsReady) {
	                for (var offset = 0; offset < nWordsReady; offset += blockSize) {
	                    // Perform concrete-algorithm logic
	                    this._doProcessBlock(dataWords, offset);
	                }

	                // Remove processed words
	                var processedWords = dataWords.splice(0, nWordsReady);
	                data.sigBytes -= nBytesReady;
	            }

	            // Return processed words
	            return new WordArray.init(processedWords, nBytesReady);
	        },

	        /**
	         * Creates a copy of this object.
	         *
	         * @return {Object} The clone.
	         *
	         * @example
	         *
	         *     var clone = bufferedBlockAlgorithm.clone();
	         */
	        clone: function () {
	            var clone = Base.clone.call(this);
	            clone._data = this._data.clone();

	            return clone;
	        },

	        _minBufferSize: 0
	    });

	    /**
	     * Abstract hasher template.
	     *
	     * @property {number} blockSize The number of 32-bit words this hasher operates on. Default: 16 (512 bits)
	     */
	    var Hasher = C_lib.Hasher = BufferedBlockAlgorithm.extend({
	        /**
	         * Configuration options.
	         */
	        cfg: Base.extend(),

	        /**
	         * Initializes a newly created hasher.
	         *
	         * @param {Object} cfg (Optional) The configuration options to use for this hash computation.
	         *
	         * @example
	         *
	         *     var hasher = CryptoJS.algo.SHA256.create();
	         */
	        init: function (cfg) {
	            // Apply config defaults
	            this.cfg = this.cfg.extend(cfg);

	            // Set initial values
	            this.reset();
	        },

	        /**
	         * Resets this hasher to its initial state.
	         *
	         * @example
	         *
	         *     hasher.reset();
	         */
	        reset: function () {
	            // Reset data buffer
	            BufferedBlockAlgorithm.reset.call(this);

	            // Perform concrete-hasher logic
	            this._doReset();
	        },

	        /**
	         * Updates this hasher with a message.
	         *
	         * @param {WordArray|string} messageUpdate The message to append.
	         *
	         * @return {Hasher} This hasher.
	         *
	         * @example
	         *
	         *     hasher.update('message');
	         *     hasher.update(wordArray);
	         */
	        update: function (messageUpdate) {
	            // Append
	            this._append(messageUpdate);

	            // Update the hash
	            this._process();

	            // Chainable
	            return this;
	        },

	        /**
	         * Finalizes the hash computation.
	         * Note that the finalize operation is effectively a destructive, read-once operation.
	         *
	         * @param {WordArray|string} messageUpdate (Optional) A final message update.
	         *
	         * @return {WordArray} The hash.
	         *
	         * @example
	         *
	         *     var hash = hasher.finalize();
	         *     var hash = hasher.finalize('message');
	         *     var hash = hasher.finalize(wordArray);
	         */
	        finalize: function (messageUpdate) {
	            // Final message update
	            if (messageUpdate) {
	                this._append(messageUpdate);
	            }

	            // Perform concrete-hasher logic
	            var hash = this._doFinalize();

	            return hash;
	        },

	        blockSize: 512/32,

	        /**
	         * Creates a shortcut function to a hasher's object interface.
	         *
	         * @param {Hasher} hasher The hasher to create a helper for.
	         *
	         * @return {Function} The shortcut function.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var SHA256 = CryptoJS.lib.Hasher._createHelper(CryptoJS.algo.SHA256);
	         */
	        _createHelper: function (hasher) {
	            return function (message, cfg) {
	                return new hasher.init(cfg).finalize(message);
	            };
	        },

	        /**
	         * Creates a shortcut function to the HMAC's object interface.
	         *
	         * @param {Hasher} hasher The hasher to use in this HMAC helper.
	         *
	         * @return {Function} The shortcut function.
	         *
	         * @static
	         *
	         * @example
	         *
	         *     var HmacSHA256 = CryptoJS.lib.Hasher._createHmacHelper(CryptoJS.algo.SHA256);
	         */
	        _createHmacHelper: function (hasher) {
	            return function (message, key) {
	                return new C_algo.HMAC.init(hasher, key).finalize(message);
	            };
	        }
	    });

	    /**
	     * Algorithm namespace.
	     */
	    var C_algo = C.algo = {};

	    return C;
	}(Math));


	return CryptoJS;

}));;(function (root, factory) {
	if (typeof exports === "object") {
		// CommonJS
		module.exports = exports = factory(require("./core"));
	}
	else if (typeof define === "function" && define.amd) {
		// AMD
		define(["./core"], factory);
	}
	else {
		// Global (browser)
		factory(root.CryptoJS);
	}
}(this, function (CryptoJS) {

	(function (Math) {
	    // Shortcuts
	    var C = CryptoJS;
	    var C_lib = C.lib;
	    var WordArray = C_lib.WordArray;
	    var Hasher = C_lib.Hasher;
	    var C_algo = C.algo;

	    // Initialization and round constants tables
	    var H = [];
	    var K = [];

	    // Compute constants
	    (function () {
	        function isPrime(n) {
	            var sqrtN = Math.sqrt(n);
	            for (var factor = 2; factor <= sqrtN; factor++) {
	                if (!(n % factor)) {
	                    return false;
	                }
	            }

	            return true;
	        }

	        function getFractionalBits(n) {
	            return ((n - (n | 0)) * 0x100000000) | 0;
	        }

	        var n = 2;
	        var nPrime = 0;
	        while (nPrime < 64) {
	            if (isPrime(n)) {
	                if (nPrime < 8) {
	                    H[nPrime] = getFractionalBits(Math.pow(n, 1 / 2));
	                }
	                K[nPrime] = getFractionalBits(Math.pow(n, 1 / 3));

	                nPrime++;
	            }

	            n++;
	        }
	    }());

	    // Reusable object
	    var W = [];

	    /**
	     * SHA-256 hash algorithm.
	     */
	    var SHA256 = C_algo.SHA256 = Hasher.extend({
	        _doReset: function () {
	            this._hash = new WordArray.init(H.slice(0));
	        },

	        _doProcessBlock: function (M, offset) {
	            // Shortcut
	            var H = this._hash.words;

	            // Working variables
	            var a = H[0];
	            var b = H[1];
	            var c = H[2];
	            var d = H[3];
	            var e = H[4];
	            var f = H[5];
	            var g = H[6];
	            var h = H[7];

	            // Computation
	            for (var i = 0; i < 64; i++) {
	                if (i < 16) {
	                    W[i] = M[offset + i] | 0;
	                } else {
	                    var gamma0x = W[i - 15];
	                    var gamma0  = ((gamma0x << 25) | (gamma0x >>> 7))  ^
	                                  ((gamma0x << 14) | (gamma0x >>> 18)) ^
	                                   (gamma0x >>> 3);

	                    var gamma1x = W[i - 2];
	                    var gamma1  = ((gamma1x << 15) | (gamma1x >>> 17)) ^
	                                  ((gamma1x << 13) | (gamma1x >>> 19)) ^
	                                   (gamma1x >>> 10);

	                    W[i] = gamma0 + W[i - 7] + gamma1 + W[i - 16];
	                }

	                var ch  = (e & f) ^ (~e & g);
	                var maj = (a & b) ^ (a & c) ^ (b & c);

	                var sigma0 = ((a << 30) | (a >>> 2)) ^ ((a << 19) | (a >>> 13)) ^ ((a << 10) | (a >>> 22));
	                var sigma1 = ((e << 26) | (e >>> 6)) ^ ((e << 21) | (e >>> 11)) ^ ((e << 7)  | (e >>> 25));

	                var t1 = h + sigma1 + ch + K[i] + W[i];
	                var t2 = sigma0 + maj;

	                h = g;
	                g = f;
	                f = e;
	                e = (d + t1) | 0;
	                d = c;
	                c = b;
	                b = a;
	                a = (t1 + t2) | 0;
	            }

	            // Intermediate hash value
	            H[0] = (H[0] + a) | 0;
	            H[1] = (H[1] + b) | 0;
	            H[2] = (H[2] + c) | 0;
	            H[3] = (H[3] + d) | 0;
	            H[4] = (H[4] + e) | 0;
	            H[5] = (H[5] + f) | 0;
	            H[6] = (H[6] + g) | 0;
	            H[7] = (H[7] + h) | 0;
	        },

	        _doFinalize: function () {
	            // Shortcuts
	            var data = this._data;
	            var dataWords = data.words;

	            var nBitsTotal = this._nDataBytes * 8;
	            var nBitsLeft = data.sigBytes * 8;

	            // Add padding
	            dataWords[nBitsLeft >>> 5] |= 0x80 << (24 - nBitsLeft % 32);
	            dataWords[(((nBitsLeft + 64) >>> 9) << 4) + 14] = Math.floor(nBitsTotal / 0x100000000);
	            dataWords[(((nBitsLeft + 64) >>> 9) << 4) + 15] = nBitsTotal;
	            data.sigBytes = dataWords.length * 4;

	            // Hash final blocks
	            this._process();

	            // Return final computed hash
	            return this._hash;
	        },

	        clone: function () {
	            var clone = Hasher.clone.call(this);
	            clone._hash = this._hash.clone();

	            return clone;
	        }
	    });

	    /**
	     * Shortcut function to the hasher's object interface.
	     *
	     * @param {WordArray|string} message The message to hash.
	     *
	     * @return {WordArray} The hash.
	     *
	     * @static
	     *
	     * @example
	     *
	     *     var hash = CryptoJS.SHA256('message');
	     *     var hash = CryptoJS.SHA256(wordArray);
	     */
	    C.SHA256 = Hasher._createHelper(SHA256);

	    /**
	     * Shortcut function to the HMAC's object interface.
	     *
	     * @param {WordArray|string} message The message to hash.
	     * @param {WordArray|string} key The secret key.
	     *
	     * @return {WordArray} The HMAC.
	     *
	     * @static
	     *
	     * @example
	     *
	     *     var hmac = CryptoJS.HmacSHA256(message, key);
	     */
	    C.HmacSHA256 = Hasher._createHmacHelper(SHA256);
	}(Math));


	return CryptoJS.SHA256;

}));(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        objectArgs = ['id', 'objectType', 'definition'];

    //ActivityDefinition, Agent/Group, Sub-Statement, or Statement Reference.
    function isString(str) {
        return typeof str === 'string';
    }

    function isInteraction(item) {
        return typeof item === 'string' && typeValidator.interactionType.test(item);
    }

    function isInteractionComponent(component) {
        return InteractionComponent.is(component);
    }

    /**
     * Instantiates a Activity
     * @param {String} id
     * @param {String} objectType
     * @param {ActivityDefinition} definition
     * @returns {Activity}
     * @constructor
     */
    function Activity(id, objectType, definition) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Activity.create(arguments[0]);
        }
        if (!(Activity.is(this))) {
            return new Activity(id, objectType, definition);
        }
        if (!id) {
            throw new Error('id is required', errorCode.ACTIVITY_MISSING_ID);
        }
        if (!(typeof id === 'string' && typeValidator.uri.test(id))) {
            throw new Error('Object with type Activity id must have an IRI', errorCode.ACTIVITY_INVALID_TYPE_ID);
        }
        this.id = id;
        if (!objectType || typeValidator.activity.test(objectType) || typeValidator.activityCase.test(objectType)) {
            if (typeValidator.activityCase.test(objectType)) {
                this.objectType = objectType.charAt(0).toUpperCase() + objectType.slice(1).toLowerCase();
            } else {
                this.objectType = objectType;
            }
        } else {
            throw new Error('Activity Invalid objectType', errorCode.ACTIVITY_INVALID_OBJECT_TYPE);
        }

        this.setDefinition(definition);
    }

    Activity.prototype.setDefinition = function (definition) {
        if (definition) {
            if (ActivityDefinition.is(definition)) {
                this.definition = definition;
            } else if (typeof definition === 'object') {
                this.definition = ActivityDefinition.create(definition);
            } else {
                throw new Error('Object Invalid definition', errorCode.ACTIVITY_INVALID_DEFINITION);
            }
        }
    };

    // static methods
    /**
     * Test if the object is an instance of Activity
     * @param object
     * @returns {boolean}
     */
    Activity.is = function (object) {
        return object instanceof Activity;
    };

    /**
     * Creates a statement Object
     * @param {object} object
     * @returns {Activity}
     */
    Activity.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a statementObject');
        }
        var args = [];
        objectArgs.forEach(function (arg) {
            if (arg in object && (arg[object] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            switch (arg) {
                case 'definition':
                    args.push(object[arg] ? ActivityDefinition.create(object[arg]) : undefined);
                    break;
                case 'objectType':
                    //Activity is the assumed default
                    args.push(object[arg] ? object[arg] : 'Activity');
                    break;
                default:
                    args.push(object[arg]);
            }
        });
        return Activity.apply(this, args);
    };

    xapi.Activity = Activity;

    var definitionArgs = ['name', 'description', 'type', 'moreInfo', 'interactionType', 'correctResponsesPattern', 'choices', 'scale', 'source', 'target', 'steps', 'extensions'];

    /**
     * Creates an instance of a ActivityDefinition
     * @param {name} name
     * @param {object} description
     * @param {string} type
     * @param {string} moreInfo
     * @param {object} interactionType
     * @param {Array} correctResponsesPattern
     * @param {Array} choices
     * @param {Array} scale
     * @param {Array} source
     * @param {Array} target
     * @param {Array} steps
     * @param {object} extensions
     * @returns {ActivityDefinition}
     * @constructor
     */
    function ActivityDefinition(name, description, type, moreInfo, interactionType, correctResponsesPattern, choices, scale, source, target, steps, extensions) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return ActivityDefinition.create(arguments[0]);
        }

        if (!(ActivityDefinition.is(this))) {
            return new ActivityDefinition(name, description, type, moreInfo, interactionType, correctResponsesPattern, choices, scale, source, target, steps, extensions);
        }
        if (name) {
            if (typeof name === 'object') {
                this.name = name;
            } else {
                throw new Error('Invalid name type', errorCode.DEFINITION_INVALID_NAME_TYPE);
            }
        }

        if (description) {
            if (typeof description === 'object') {
                this.description = description;
            } else {
                throw new Error('Invalid description type', errorCode.DEFINITION_INVALID_DESCRIPTION_TYPE);
            }
        }

        if (type) {
            if (typeof type === 'string' && typeValidator.uri.test(type)) {
                this.type = type;
            } else {
                throw new Error('Invalid type type', errorCode.DEFINITION_INVALID_TYPE_TYPE);
            }
        }

        if (moreInfo) {
            if (typeof moreInfo === 'string' && typeValidator.uri.test(moreInfo)) {
                this.moreInfo = moreInfo;
            } else {
                throw new Error('Invalid moreInfo type', errorCode.DEFINITION_INVALID_MORE_INFO_TYPE);
            }
        }

        if (interactionType) {
            if (isInteraction(interactionType)) {
                this.interactionType = interactionType;
            } else {
                throw new Error('Invalid interactionType', errorCode.DEFINITION_INVALID_INTERACTION_TYPE);
            }
        }

        if (correctResponsesPattern) {
            if (correctResponsesPattern instanceof Array && correctResponsesPattern.every(isString)) {
                this.correctResponsesPattern = correctResponsesPattern;
            } else {
                throw new Error('Invalid correctResponsesPattern type', errorCode.DEFINITION_INVALID_INTERACTION_TYPE);
            }

        }

        if (choices) {
            if (choices instanceof Array && choices.every(isInteractionComponent)) {
                this.choices = choices;
            } else {
                throw new Error('Invalid choices type', errorCode.DEFINITION_INVALID_CHOICES_TYPE);
            }
        }

        if (scale) {
            if (scale instanceof Array && scale.every(isInteractionComponent)) {
                this.scale = scale;
            } else {
                throw new Error('Invalid scale type', errorCode.DEFINITION_INVALID_SCALE_TYPE);
            }
        }

        if (source) {
            if (source instanceof Array && source.every(isInteractionComponent)) {
                this.source = source;
            } else {
                throw new Error('Invalid source type', errorCode.DEFINITION_INVALID_SOURCE_TYPE);
            }
        }

        if (target) {
            if (target instanceof Array && target.every(isInteractionComponent)) {
                this.target = target;
            } else {
                throw new Error('Invalid target type', errorCode.DEFINITION_INVALID_TARGET_TYPE);
            }
        }

        if (steps) {
            if (steps instanceof Array && steps.every(isInteractionComponent)) {
                this.steps = steps;
            } else {
                throw new Error('Invalid step type', errorCode.DEFINITION_INVALID_STEP_TYPE);
            }
        }

        if (extensions) {
            if (typeof extensions === 'object' && typeValidator.extensions.test(extensions)) {
                this.extensions = extensions;
            } else {
                throw new Error('Invalid extensions type', errorCode.DEFINITION_INVALID_EXTENSIONS_TYPE);
            }
        }
    }

    //static methods
    /**
     * Test if the object is an instance of ActivityDefinition
     * @param {object} object
     * @returns {boolean}
     */
    ActivityDefinition.is = function (object) {
        return object instanceof ActivityDefinition;
    };

    /**
     * Creates an instance of a definition
     * @param object
     * @returns {ActivityDefinition}
     */
    ActivityDefinition.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a definition');
        }
        var args = [];
        definitionArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                case 'choices':
                case 'scale':
                case 'source':
                case 'target':
                case 'steps':
                    if (object[arg] && !(object[arg] instanceof Array)) {
                        throw new Error('Interaction choices component must be an array');
                    }
                    args.push(object[arg] ? object[arg].map(function (item) {
                        return InteractionComponent.create(item);
                    }) : undefined);
                    break;
                default:
                    args.push(object[arg]);
            }

        });
        return ActivityDefinition.apply(this, args);
    };

    xapi.ActivityDefinition = ActivityDefinition;

    var interactionComponentArgs = ['id', 'description'];

    /**
     * InteractionComponent constructor
     * @param id
     * @param description
     * @constructor
     */
    function InteractionComponent(id, description) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return InteractionComponent.create(arguments[0]);
        }

        if (!(InteractionComponent.is(this))) {
            return new InteractionComponent(id, description);
        }

        if (!id) {
            throw new Error('id is required', errorCode.INTERACTION_COMPONENT_ID);
        }
        if ((typeof id !== 'string')) {
            throw new Error('Object with type InteractionComponent id must be type String', errorCode.INTERACTION_COMPONENT_TYPE_ID);
        }

        this.id = id;
        if (description) {
            if (typeof description === 'object') {
                this.description = description;
            } else {
                throw new Error('Invalid description type', errorCode.INTERACTION_COMPONENT_INVALID_DESCRIPTION_TYPE);
            }
        }
    }

    InteractionComponent.is = function (object) {
        return object instanceof InteractionComponent;
    };

    InteractionComponent.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create an interaction component');
        }

        var args = [];
        interactionComponentArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            args.push(object[arg]);
        });

        return InteractionComponent.apply(this, args);
    };

    xapi.InteractionComponent = InteractionComponent;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,

        attachmentArgs = ['usageType', 'display', 'contentType', 'length', 'sha2', 'description', 'fileUrl'];

    /**
     * Creates an instance of Attatchment
     * @param {string} usageType
     * @param {object} display
     * @param {string} contentType
     * @param {number} length
     * @param {string} sha2
     * @param {object} description
     * @param {string} fileUrl
     * @returns {Attachment}
     * @constructor
     */
    function Attachment(usageType, display, contentType, length, sha2, description, fileUrl) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Attachment.create(arguments[0]);
        }
        if (!(Attachment.is(this))) {
            return new Attachment(usageType, display, contentType, length, sha2, description, fileUrl);
        }

        if (usageType) {
            if (typeValidator.uri.test(usageType)) {
                this.usageType = usageType;
            } else {
                throw new Error('Invalid usageType', errorCode.ATTACHMENT_USAGE_TYPE_NOT_IRI);
            }
        } else {
            throw new Error('usageType is required', errorCode.ATTACHMENT_USAGE_TYPE_REQUIRED);
        }

        if (display) {
            if (typeof display === 'object') {
                this.display = display;
            } else {
                throw new Error('Invalid display', errorCode.ATTACHMENT_INVALID_DISPLAY_TYPE);
            }
        } else {
            throw new Error('display is required', errorCode.ATTACHMENT_USAGE_TYPE_REQUIRED);
        }

        if (contentType) {
            if (typeof contentType === 'string') {
                this.contentType = contentType;
            } else {
                throw new Error('invalid contentType type', errorCode.ATTACHMENT_INVALID_CONTENT_TYPE_TYPE);
            }
        } else {
            throw new Error('contentType is required', errorCode.ATTACHMENT_CONTENT_TYPE_REQUIRED);
        }

        if (length) {
            if (typeof length === 'number') {
                this.length = length;
            } else {
                throw new Error('invalid length type', errorCode.ATTACHMENT_INVALID_LENGTH_TYPE);
            }
        } else {
            throw new Error('length is required', errorCode.ATTACHMENT_LENGTH_REQUIRED);
        }

        if (sha2) {
            if (typeof sha2 === 'string') {
                this.sha2 = sha2;
            } else {
                throw new Error('invalid sha2 type', errorCode.ATTACHMENT_INVALID_SHA2_TYPE);
            }
        } else {
            throw new Error('sha2 is required', errorCode.ATTACHMENT_SHA2_REQUIRED);
        }

        if (description) {
            if (typeof description === 'object') {
                this.description = description;
            } else {
                throw new Error('invalid descriptoind type', errorCode.ATTACHMENT_INVALID_DESCRIPTION_TYPE);
            }
        }

        if (fileUrl) {
            if (typeValidator.uri.test(fileUrl)) {
                this.fileUrl = fileUrl;
            } else {
                throw new Error('invalid fileUrl type', errorCode.ATTACHMENT_FILE_URL_NOT_IRI);
            }
        }
    }

    //static methods
    /**
     * Test if the object is an instance of Attachment
     * @param object
     * @returns {boolean}
     */
    Attachment.is = function (object) {
        return object instanceof Attachment;
    };

    /**
     * creates an Attachment or an array of Attachments
     * @param objectOrArray
     * @returns {Attachment | [Attachment]}
     */
    Attachment.create = function (objectOrArray) {
        if (!objectOrArray || typeof objectOrArray !== 'object') {
            throw new Error('object is required to create an attachment');
        }
        function factory(item, index) {
            var args = [],
                object = arguments.length === 2 ? objectOrArray[index] : objectOrArray;
            attachmentArgs.forEach(function (arg) {

                if (arg in object && (object[arg] === undefined || object[arg] === null)) {
                    throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
                }

                args.push(item[arg]);
            });

            return Attachment.apply(this, args);
        }

        if (objectOrArray instanceof Array) {
            return objectOrArray.map(factory);
        } else {
            return factory(objectOrArray);
        }
    };

    xapi.Attachment = Attachment;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));
// jscs: disable requireCamelCaseOrUpperCaseIdentifiers
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        identifierTypeValidator = {
            mbox: typeValidator.mailTo,
            mbox_sha1sum: {
                test: function (value) {
                    return typeof value === 'string';
                }
            },
            openid: typeValidator.uri,
            account: {
                test: function (account) {
                    if (account) {
                        return account.homePage && typeValidator.uri.test(account.homePage)
                            && account.name && typeof account.name === 'string';
                    }
                    return true;
                }
            }
        },
        actorArgs = ['objectType', 'name', 'mbox', 'mbox_sha1sum', 'openid', 'account', 'member'],
        personArgs = actorArgs.slice(1, (actorArgs.length - 1)),
        identifierError = {
            mbox: errorCode.AGENT_IDENTIFIER_INVALID_MBOX,
            mbox_sha1sum: errorCode.AGENT_IDENTIFIER_INVALID_MBOX_SHA1SUM,
            openid: errorCode.AGENT_IDENTIFIER_INVALID_OPENID,
            account: errorCode.AGENT_IDENTIFIER_INVALID_ACCOUNT_TYPE
        };

    /**
     * Creates an instance of an Actor
     * @param {string} objectType
     * @param {string} name
     * @param {string} mbox
     * @param {string} mbox_sha1sum
     * @param {string} openid
     * @param {object} account
     * @param {Array} members
     * @returns {Actor}
     * @constructor
     */
    function Actor(objectType, name, mbox, mbox_sha1sum, openid, account, members) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Actor.create(arguments[0]);
        }
        if (!Actor.is(this)) {
            return new Actor(objectType, name, mbox, mbox_sha1sum, openid, account, members);
        }
        if (objectType) {
            if (!typeValidator.agentOrGroup.test(objectType)) {
                if (!typeValidator.agentOrGroupCase.test(objectType)) {
                    throw new Error('Actor must be an agent or group', errorCode.AGENT_INVALID_OBJECT_TYPE);
                } else {
                    // Capitalize first letter
                    objectType = objectType.charAt(0).toUpperCase() + objectType.slice(1).toLowerCase();
                }
            }
            this.objectType = objectType;
        }
        if (members) {
            if (members instanceof Array) {
                this.member = [];
                for (var argIndex = 0; argIndex < members.length; ++argIndex) {
                    var agent = members[argIndex];
                    if (Actor.is(agent)) {
                        this.member.push(agent);
                    } else {
                        this.member.push(Actor.create(agent));
                    }
                }
            } else {
                throw new Error('Member must be an array of agents', errorCode.AGENT_INVALID_MEMBER_TYPE);
            }
        }
        //test that we have an identifier
        var identifierCount = 0,
            identifier;
        for (var argIndex = 2; argIndex < 6; ++argIndex) {
            var key = actorArgs[argIndex];
            if (arguments[argIndex]) {
                identifierCount++;
                if (identifierCount > 1) {
                    throw new Error('Only one inverseIdentifier may be used', errorCode.AGENT_MORE_THAT_ONE_IDENTIFIER);
                }
                if (!(identifierTypeValidator[key]).test(arguments[argIndex])) {
                    throw new Error('Actor invalid identifier type for: ' + key, identifierError[key]);
                }
                this[key] = arguments[argIndex];
                identifier = arguments[argIndex];
            }
        }
        if (identifierCount === 0 && !members) {
            throw new Error('InverseIdentifier is required', errorCode.MISSING_IDENTIFIER);
        }
        if (name) {
            if (typeof name !== 'string') {
                throw new Error('Name must be of type "string"', errorCode.AGENT_INVALID_NAME_TYPE);
            }
            this.name = name;
        }

        this.getIdentifier = function () {
            return identifier;
        };
    }

    /**
     * Tests if the actor is an agent
     * @returns {boolean}
     */
    Actor.prototype.isAgent = function (element) {
        if (!element) {
            element = this;
        }
        return typeof element.objectType === 'string' && (typeValidator.agent.test(element.objectType) || typeValidator.agentCase.test(element.objectType));
    };

    //static methods
    /**
     * Test if object is an instance of Actor
     * @param {object} object
     * @returns {boolean}
     */
    Actor.is = function (object) {
        return object instanceof Actor;
    };

    /**
     * Creates an instance of actor
     * @param object
     * @returns {Actor}
     */
    Actor.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create an actor');
        }
        var args = [];
        actorArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            args.push(object[arg]);
        });
        return Actor.apply(this, args);
    };

    /**
     * Creates an instance of a person
     * @param {String} name
     * @param {[string]} mbox
     * @param {[string]} mbox_sha1sum
     * @param {[string]} openid
     * @param {[Object]} account
     * @returns {*}
     * @constructor
     */
    function Person(agents) {
        if (!Person.is(this)) {
            return new Person(agents);
        }
        this.objectType = 'Person';
        if (!agents || !(agents instanceof Array)) {
            throw new Error('an Array of object is required to create a person object', errorCode.AGENT_INVALID_OBJECT_TYPE);
        }
        agents.forEach(this.add.bind(this));
    }

    Person.prototype.add = function (agt) {
        var agent;
        if (Actor.is(agt)) {
            agent = agt;
        } else {
            agent = Actor.create(agt);
        }
        personArgs.forEach(function (propertyName) {
            if (propertyName in agent && agent[propertyName]) {
                (propertyName in this ? this[propertyName] : this[propertyName] = []).push(agent[propertyName]);
            }
        }.bind(this));
    };

    /**
     * Test if an object is an instance of Persion
     * @param {Object} object
     * @returns {boolean}
     */
    Person.is = function (object) {
        return object instanceof Person;
    };

    /**
     * Creates a Person instance from an object
     * @param [object]
     * @returns {Actor}
     */
    Person.create = function (object) {
        if (!object || typeof object !== 'object' || !object instanceof Array) {
            throw new Error('an Array of object is required to create a person object', errorCode.AGENT_INVALID_OBJECT_TYPE);
        }

        return Person.call(this, object);
    };

    xapi.Person = Person;
    xapi.Actor = Actor;
}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));

(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        resultArgs = ['score', 'success', 'completion', 'response', 'duration', 'extensions'];

    /**
     * Resturns and instance of a Result object
     * @param {Score} score
     * @param {boolean} success
     * @param {boolean} completion
     * @param response
     * @param duration
     * @param extensions
     * @returns {Result}
     * @constructor
     */
    function Result(score, success, completion, response, duration, extensions) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Result.create(arguments[0]);
        }
        if (!(Result.is(this))) {
            return new Result(score, success, completion, response, duration, extensions);
        }
        if (score) {
            if (Score.is(score)) {
                this.score = score;
            } else {
                throw new Error('Result invalid score type', errorCode.RESULTS_INVALID_SCORE);
            }
        }

        if (success !== undefined) {
            if (typeof success === 'boolean') {
                this.success = success;
            } else {
                throw new Error('Result invalid success type : ' + success, errorCode.RESULTS_INVALID_SUCCESS);
            }
        }

        if (completion !== undefined) {
            if (typeof completion === 'boolean') {
                this.completion = completion;
            } else {
                throw new Error('Result invalid completion type : ' + completion, errorCode.RESULTS_INVALID_COMPLETION);
            }
        }

        if (response) {
            if (typeof response === 'string') {
                this.response = response;
            } else {
                throw new Error('Result invalid response type : ' + response, errorCode.RESULTS_INVALID_RESPONSE);
            }
        }

        if (duration) {
            if (duration instanceof Date) {
                this.duration = duration.toISOString();
            } else if (typeof duration === 'string' && typeValidator.isoDuration.test(duration)) {
                this.duration = duration;
            } else {
                throw new Error('Result invalid duration type : ' + duration, errorCode.RESULTS_INVALID_DURATION);
            }
        }

        if (extensions) {
            if (typeof extensions === 'object' && typeValidator.extensions.test(extensions)) {
                this.extensions = extensions;
            } else {
                throw new Error('Result invalid extensions type : ' + extensions, errorCode.RESULTS_INVALID_EXTENSIONS);
            }
        }

    }

    // static methods
    /**
     * Test if the object is an instance of Result
     * @param object
     * @returns {boolean}
     */
    Result.is = function (object) {
        return object instanceof Result;
    };

    /**
     * Creates an Result instance
     * @param object
     * @returns {Result}
     */
    Result.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('Result object is required to create a result');
        }
        var args = [];
        resultArgs.forEach(function (arg) {
            if (arg in object && (arg[object] === null)) {
                throw new Error('Result property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            if (arg in object && arg === 'score') {
                object[arg] = new Score(object[arg]);
            }
            args.push(arg in object ? object[arg] : undefined);
        });
        return Result.apply(this, args);
    };

    var scoreArgs = ['scaled', 'raw', 'min', 'max'];

    /**
     * Instanciates a score object
     * @param {number} scaled
     * @param {number} raw
     * @param {number} min
     * @param {number} max
     * @constructor
     */
    function Score(scaled, raw, min, max) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Score.create(arguments[0]);
        }
        if (!(Score.is(this))) {
            return new Score(scaled, raw, min, max);
        }
        if (scaled !== undefined) {
            if (typeof scaled === 'number' && scaled >= -1 && scaled <= 1) {
                this.scaled = scaled;
            } else {
                throw new Error('Score invalid scaled type ' + scaled, errorCode.RESULTS_SCORE_INVALID_SCALED);
            }
        }

        if (raw !== undefined) {
            if (typeof raw === 'number') {
                this.raw = raw;
            } else {
                throw new Error('Score invalid raw type ' + raw, errorCode.RESULTS_SCORE_INVALID_RAW);
            }
        }

        if (min !== undefined) {
            if (typeof min === 'number') {
                this.min = min;
            } else {
                throw new Error('Score invalid min type ' + min, errorCode.RESULTS_SCORE_INVALID_MIN);
            }
        }

        if (max !== undefined) {
            if (typeof max === 'number') {
                this.max = max;
            } else {
                throw new Error('Score invalid max type ' + max, errorCode.RESULTS_SCORE_INVALID_MAX);
            }
        }

        if (this.min && this.max) {
            if (this.min > this.max) {
                throw new Error('Score min ' + this.min + ' cannot be greater than max ' + this.max, errorCode.RESULTS_SCORE_INVALID_MIN_MAX);
            }
        }

        if (this.raw && this.min) {
            if (this.raw < this.min) {
                throw new Error('Score raw ' + this.raw + ' cannot be less than min ' + this.min, errorCode.RESULTS_SCORE_INVALID_RAW);
            }
        }

        if (this.raw && this.max) {
            if (this.raw > this.max) {
                throw new Error('Score raw ' + this.raw + ' cannot be greater than max ' + this.max, errorCode.RESULTS_SCORE_INVALID_RAW);
            }
        }
    }

    /**
     * test if the object is an instance of Score
     * @param object
     * @returns {boolean}
     */
    Score.is = function (object) {
        return object instanceof Score;
    };

    /**
     * Creates an instance of score
     * @param object
     * @returns {*}
     */
    Score.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('Score object is required to create a score');
        }
        var args = [];
        scoreArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === undefined || object[arg] === null)) {
                throw new Error('Score property ' + arg + ' is null or undefined. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            args.push(arg in object ? object[arg] : undefined);
        });
        return Score.apply(this, args);
    };

    xapi.Result = Result;
    xapi.Score = Score;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        documentArgs = ['id', 'updated', 'contents'];

    /**
     * Document constructor
     * @param {String} id
     * @param {Timestamp} updated
     * @param {String} contents
     * @returns {Document}
     * @constructor
     */
    function Document(id, updated, contents) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Document.create(arguments[0]);
        }
        if (!(Document.is(this))) {
            return new Document(id, updated, contents);
        }

        if (id) {
            if (typeof id === 'string') {
                this.id = id;
            } else {
                throw new Error('invalid document id', errorCode.DOCUMENT_ID_INVALID);
            }
        }

        if (updated) {
            if (updated instanceof Date) {
                this.updated = updated.toISOString();
            } else if (typeof updated === 'string' && typeValidator.isoDate.test(updated)) {
                this.updated = updated;
            } else {
                throw new Error('invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        } else {
            this.updated = (new Date()).toISOString();
        }

        if (contents) {
            this.contents = contents;
        } else {
            throw new Error('document contents required', errorCode.DOCUMENT_CONTENTS_REQUIRED);
        }
    }

    /**
     * Test that the object is an instance of Document
     * @param object
     * @returns {boolean}
     */
    Document.is = function (object) {
        return object instanceof Document;
    };

    /**
     * Creates an instance of Document
     * @param object
     * @returns {Document}
     */
    Document.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a Document');
        }
        var args = [];
        documentArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                default:
                    args.push(object[arg]);
            }
        });
        return Document.apply(this, args);
    };

    xapi.Document = Document;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        statementRefArgs = ['objectType', 'id'];

    /**
     * Statement Reference instance
     * @param {string} objectType - object type must be 'StatementRef'
     * @param {string} id - id must be the uuid of an existing statement;
     * @returns {*}
     * @constructor
     */
    function StatementRef(objectType, id) {
        if (arguments.length === 1 && typeof name === 'object') {
            return StatementRef.create(objectType);
        }
        if (!(StatementRef.is(this))) {
            return new StatementRef(objectType, id);
        }
        if (objectType) {
            if (!typeValidator.statementref.test(objectType)) {
                if (typeValidator.statementrefCase.test(objectType)) {
                    objectType = 'StatementRef'; // Must be in this format
                } else {
                    throw new Error('StatementRef must have an objectType "StatementRef"', errorCode.STATEMENT_REF_INVALID_OBJECT_TYPE);
                }
            }
            this.objectType = objectType;
        } else {
            throw new Error('StatemenRef missing objectType', errorCode.STATEMENT_REF_MISSING_OBJECT_TYPE);
        }

        if (id) {
            if (!typeValidator.uuid.test(id)) {
                throw new Error('StatementRef id must be a uuid', errorCode.STATEMENT_REF_INVALID_ID);
            }
            this.id = id;

        } else {
            throw new Error('StatementRef missing id', errorCode.STATEMENT_REF_MISSING_ID);
        }
    }

    /**
     * Test if an object is an instance of StatementRef
     * @param object
     * @returns {boolean}
     */
    StatementRef.is = function (object) {
        return object instanceof StatementRef;
    };

    /**
     * Factory that will create StatementRef instances from an object
     * @param object
     * @returns {*}
     */
    StatementRef.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a StatementRef');
        }
        var args = [];
        statementRefArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            args.push(object[arg]);
        });

        return StatementRef.apply(this, args);
    };

    xapi.StatementRef = StatementRef;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        verbArgs = ['id', 'display'];

    /**
     * Verb constructor id must be an iri
     * @param {String} id
     * @param {Object} display
     * @returns {Verb}
     * @constructor
     */
    function Verb(id, display) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Verb.create(arguments[0]);
        }
        if (!(Verb.is(this))) {
            return new Verb(id, display);
        }
        if (!(typeValidator.uri.test(id))) {
            throw new Error('Invalid id must be an IRI', errorCode.VERB_ID_NOT_IRI);
        }

        this.id = id;

        if (display) {
            if (typeof display === 'object') {
                this.display = display;
            } else {
                throw new Error('Invalid Display Type', errorCode.VERB_INVALID_DISPLAY_TYPE);
            }
        }
    }

    /**
     * Test that the object is an instance of Verb
     * @param object
     * @returns {boolean}
     */
    Verb.is = function (object) {
        return object instanceof Verb;
    };

    /**
     * Creates an instance of Verb
     * @param object
     * @returns {Verb}
     */
    Verb.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a verb');
        }
        var args = [];
        verbArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            args.push(object[arg]);
        });

        return Verb.apply(this, args);
    };

    xapi.Verb = Verb;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : Error,
        Activity = require ? require('./activity').Activity : xapi.Activity,
        Actor = require ? require('./actor').Actor : xapi.Actor,
        StatementRef = require ? require('./statementRef').StatementRef : xapi.StatementRef,
        contextArgs = ['registration', 'instructor', 'team', 'contextActivities', 'revision', 'platform', 'language', 'statement', 'extensions'],
        contextActivitiesProperties = ['parent', 'grouping', 'category', 'other'];

    function isContextActivities(object) {
        if (typeof object !== 'object') {
            return false;
        }
        for (var key in contextActivitiesProperties) {
            if (contextActivitiesProperties[key] in object) {
                var activity = object[contextActivitiesProperties[key]];
                if (!activity || (Array.isArray(activity) && activity.length === 0)) {
                    //every value must be either single Activity Object or an array of Activity Objects
                    //https://github.com/adlnet/xAPI-Spec/blob/master/xAPI.md#requirements-11
                    return false;
                }
            }
        }
        for (var key in object) {
            var activity = object[key];
            if (contextActivitiesProperties.indexOf(key) < 0) {
                return false;
            } else if (activity instanceof Array && !activity.every(Activity.is)) {
                return false;
            } else if (!activity instanceof Array && !Activity.is(activity)) {
                return false;
            }
        }
        return true;
    }

    function isStatement(object) {
        return object.objectType && object.objectType.toLowerCase() === 'statementref' && typeof object.id === 'string';
    }

    /**
     * Create an instance of a context object
     * @param {string} registration
     * @param {object} instructor
     * @param {object} team
     * @param {object} contextActivities
     * @param {string} revision
     * @param {string} platform
     * @param {string} language
     * @param {object} statement
     * @param extensions
     * @returns {Context}
     * @constructor
     */
    function Context(registration, instructor, team, contextActivities, revision, platform, language, statement, extensions) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Context.create(arguments[0]);
        }
        if (!(Context.is(this))) {
            return new Context(registration, instructor, team, contextActivities, revision, platform, language, statement, extensions);
        }
        if (registration) {
            if (typeof registration === 'string' && typeValidator.uuid.test(registration)) {
                this.registration = registration;
            } else {
                throw new Error('Context Invalid registration type', errorCode.CONTEXT_INVALID_REGISTRATION_TYPE);
            }

        }
        if (instructor) {
            if (Actor.is(instructor)) {
                this.instructor = instructor;
            } else if (typeof instructor === 'object') {
                this.instructor = Actor.create(instructor);
            } else {
                throw new Error('Context Invalid instructor type', errorCode.CONTEXT_INVALID_INSTRUCTOR_TYPE);
            }
        }
        if (team) {
            if (Actor.is(team)) {
                this.team = team;
            } else if (typeof team === 'object') {
                this.team = Actor.create(team);
            } else {
                throw new Error('Context Invalid team type', errorCode.CONTEXT_INVALID_TEAM_TYPE);
            }
        }

        if (revision) {
            if (typeof revision === 'string') {
                this.revision = revision;
            } else {
                throw new Error('Context Invalid revision type', errorCode.CONTEXT_INVALID_REVISION_TYPE);
            }
        }

        if (contextActivities) {
            if (isContextActivities(contextActivities)) {
                this.contextActivities = contextActivities;
            } else {
                throw new Error('Context Invalid context activities type', errorCode.CONTEXT_INVALID_CONTEXT_ACTIVITY);
            }
        }

        if (platform) {
            if (typeof platform === 'string') {
                this.revision = platform;
            } else {
                throw new Error('Context Invalid platform type', errorCode.CONTEXT_INVALID_PLATFORM_TYPE);
            }
        }

        if (typeof language === 'string') {
            this.language = language;
        } else if (language) {
            throw new Error('Context Invalid language type', errorCode.CONTEXT_INVALID_LANGUAGE_TYPE);
        }

        if (statement) {
            if (StatementRef.is(statement)) {
                this.statement = statement;
            } else {
                throw new Error('Statement Context type StatementRef is invalid', errorCode.STATEMENT_INVALID_STATEMENT_REF);
            }
        }

        if (extensions) {
            if (typeof extensions === 'object' && typeValidator.extensions.test(extensions)) {
                this.extensions = extensions;
            } else {
                throw new Error('Context Invalid extentions type', errorCode.CONTEXT_INVALID_EXTENSION_TYPE);
            }
        }
    }

    //static methods
    /**
     * Test if the object is an instance of Context
     * @param object
     * @returns {boolean}
     */
    Context.is = function (object) {
        return object instanceof Context;
    };

    function toActivity(activity) {
        return Activity.create(activity);
    }

    /**
     * Creates an instacne of context
     * @param object
     * @returns {Context}
     */
    Context.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('Context object is required to create a context');
        }
        var args = [];
        contextArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('Context property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }
            switch (arg) {
                case 'contextActivities' :
                    if (object[arg]) {
                        var contextActivities = object[arg];
                        for (var property in contextActivities) {
                            if (contextActivities.hasOwnProperty(property)) {
                                var activity = contextActivities[property];
                                var array = Array.isArray(activity) ? activity : [activity];
                                contextActivities[property] = array.map(toActivity);
                            }
                        }
                        args.push(contextActivities);
                    } else {
                        args.push(undefined);
                    }
                    break;
                case 'statement' :
                    if (object[arg]) {
                        args.push(StatementRef.create(object[arg]));
                    } else {
                        args.push(undefined);
                    }
                    break;
                default :
                    args.push(object[arg]);
                    break;
            }

        });
        return Context.apply(this, args);
    };

    xapi.Context = Context;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        Actor = require ? require('./actor').Actor : xapi.Actor,
        Document = require ? require('./document').Document : xapi.Document,
        stateArgs = ['activityId', 'agent', 'registration', 'stateId', 'since', 'document'];

    /**
     * State constructor
     * @param {String} activityId
     * @param {Object} agent
     * @param {UUID} registration
     * @param {String} stateId
     * @param {Timestamp} since
     * @param {Document} document
     * @returns {State}
     * @constructor
     */
    function State(activityId, agent, registration, stateId, since, document) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return State.create(arguments[0]);
        }
        if (!(State.is(this))) {
            return new State(activityId, agent, registration, stateId, since, document);
        }

        if (activityId) {
            if (typeof activityId === 'string') {
                this.activityId = activityId;
            } else {
                throw new Error('invalid activity iri', errorCode.ACTIVITY_ID_NOT_IRI);
            }
        } else {
            throw new Error('activity id required', errorCode.ACTIVITY_ID_REQUIRED);
        }

        if (agent) {
            if (Actor.is(agent)) {
                this.agent = agent;
            } else {
                throw new Error('invalid agent or group', errorCode.AGENT_INVALID_OBJECT_TYPE);
            }
        } else {
            throw new Error('agent required', errorCode.AGENT_REQUIRED);
        }

        //Optional
        if (registration) {
            if (typeValidator.uuid.test(registration)) {
                this.registration = registration;
            } else {
                throw new Error('invalid registration type', errorCode.CONTEXT_INVALID_REGISTRATION_TYPE);
            }
        }

        //Only required for single document requests
        if (stateId) {
            if (typeof stateId === 'string') {
                this.stateId = stateId;
            } else {
                throw new Error('state id must be a string', errorCode.STATE_ID_INVALID);
            }
        }

        //optional
        if (since) {
            if (since instanceof Date) {
                this.since = since.toISOString();
            } else if (typeof since === 'string' && typeValidator.isoDate.test(since)) {
                this.since = since;
            } else {
                throw new Error('invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        }

        //TODO: Refactor this to not use Getters/Setters.
        var internalDocument;
        /**
         * Document getter
         * @returns {Object} document - returns the document object for this state
         */
        this.getDocument = function () {
            return internalDocument;
        };

        /**
         * Document setter
         * @param {Object} doc - document to be set
         */
        this.setDocument = function (doc) {
            if (doc) {
                if (Document.is(doc)) {
                    internalDocument = doc;
                } else {
                    throw new Error('invalid document', errorCode.DOCUMENT_INVALID_OBJECT_TYPE);
                }
            } else {
                // allow setting it to a falsey value.
                internalDocument = doc;
            }
        };

        // optional
        if (document) {
            this.setDocument(document);
        }
    }

    /**
     * Test that the object is an instance of State
     * @param object
     * @returns {boolean}
     */
    State.is = function (object) {
        return object instanceof State;
    };

    /**
     * Creates an instance of State
     * @param object
     * @returns {State}
     */
    State.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a state');
        }
        var args = [];
        stateArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                case 'agent':
                    args.push(arg in object ? Actor.create(object[arg]) : undefined);
                    break;
                case 'document':
                    args.push(arg in object ? Document.create(object[arg]) : undefined);
                    break;
                default:
                    args.push(object[arg]);
            }
        });
        return State.apply(this, args);
    };

    xapi.State = State;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        Actor = require ? require('./actor').Actor : xapi.Actor,
        Document = require ? require('./document').Document : xapi.Document,
        agentProfileArgs = ['agent', 'profileId', 'since', 'document'];

    /**
     * ActivityDefinition constructor
     * @param {Object} agent
     * @param {String} profileId
     * @param {Timestamp} since
     * @returns {Activity}
     * @constructor
     */
    function AgentProfile(agent, profileId, since, document) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return AgentProfile.create(arguments[0]);
        }
        if (!(AgentProfile.is(this))) {
            return new AgentProfile(agent, profileId, since, document);
        }

        if (agent && Actor.is(agent)) {
            if ('objectType' in agent && !typeValidator.agent.test(agent.objectType)) {
                if(typeValidator.agentCase.test(agent.objectType)){
                    agent.objectType = agent.objectType.charAt(0).toUpperCase() + agent.objectType.slice(1).toLowerCase();
                }else{
                    throw new Error('Invalid agent type', errorCode.AGENT_INVALID_MEMBER_TYPE);
                }

            } else {
                this.agent = agent;
            }
        } else {
            throw new Error('Agent is required', errorCode.AGENT_REQUIRED);
        }

        //Only required for single document requests so we do the required check there
        if (profileId) {
            if (typeof profileId === 'string') {
                this.profileId = profileId;
            } else {
                throw new Error('invalid profile type', errorCode.PROFILE_ID_INVALID_TYPE);
            }
        }

        //optional
        if (since) {
            if (since instanceof Date) {
                this.since = since.toISOString();
            } else if (typeof since === 'string' && typeValidator.isoDate.test(since)) {
                this.since = since;
            } else {
                throw new Error('invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        }

        //TODO: Refactor this to not use Getters/Setters.
        var internalDocument;
        /**
         * Document getter
         * @returns {Object} document - returns the document object for this state
         */
        this.getDocument = function () {
            return internalDocument;
        };

        /**
         * Document setter
         * @param {Object} doc - document to be set
         */
        this.setDocument = function (doc) {
            if (doc) {
                if (Document.is(doc)) {
                    internalDocument = doc;
                } else {
                    throw new Error('invalid document', errorCode.DOCUMENT_INVALID_OBJECT_TYPE);
                }
            } else {
                // allow setting it to a falsey value.
                internalDocument = doc;
            }
        };

        // optional
        if (document) {
            this.setDocument(document);
        }
    }

    /**
     * Test that the object is an instance of AgentProfile
     * @param object
     * @returns {boolean}
     */
    AgentProfile.is = function (object) {
        return object instanceof AgentProfile;
    };

    /**
     * Creates an instance of AgentProfile
     * @param object
     * @returns {AgentProfile}
     */
    AgentProfile.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create an agentProfile');
        }
        var args = [];
        agentProfileArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                case 'agent':
                    args.push(arg in object ? Actor.create(object[arg]) : undefined);
                    break;
                default:
                    args.push(object[arg]);
            }
        });
        return AgentProfile.apply(this, args);
    };

    xapi.AgentProfile = AgentProfile;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        Document = require ? require('./document').Document : xapi.Document,
        activityArgs = ['activityId', 'profileId', 'since', 'document'];

    /**
     * ActivityProfile constructor
     * @param {String} activityId
     * @param {String} profileId
     * @param {Timestamp} since
     * @returns {ActivityProfile}
     * @constructor
     */
    function ActivityProfile(activityId, profileId, since, document) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return ActivityProfile.create(arguments[0]);
        }
        if (!(ActivityProfile.is(this))) {
            return new ActivityProfile(activityId, profileId, since, document);
        }

        if (activityId) {
            if (typeof activityId === 'string') {
                this.activityId = activityId;
            } else {
                throw new Error('invalid activity type', errorCode.ACTIVITY_ID_INVALID_TYPE);
            }
        } else {
            throw new Error('activity id required', errorCode.ACTIVITY_ID_REQUIRED);
        }

        //Only required for single document requests so we do the required check there
        if (profileId) {
            if (typeof profileId === 'string') {
                this.profileId = profileId;
            } else {
                throw new Error('invalid profile type', errorCode.PROFILE_ID_INVALID_TYPE);
            }
        }

        //optional
        if (since) {
            if (since instanceof Date) {
                this.since = since.toISOString();
            } else if (typeof since === 'string' && typeValidator.isoDate.test(since)) {
                this.since = since;
            } else {
                throw new Error('invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        }

        //TODO: Refactor this to not use Getters/Setters.
        var internalDocument;
        /**
         * Document getter
         * @returns {Object} document - returns the document object for this state
         */
        this.getDocument = function () {
            return internalDocument;
        };

        /**
         * Document setter
         * @param {Object} doc - document to be set
         */
        this.setDocument = function (doc) {
            if (doc) {
                if (Document.is(doc)) {
                    internalDocument = doc;
                } else {
                    throw new Error('invalid document', errorCode.DOCUMENT_INVALID_OBJECT_TYPE);
                }
            } else {
                // allow setting it to a falsey value.
                internalDocument = doc;
            }
        };

        // optional
        if (document) {
            this.setDocument(document);
        }
    }

    /**
     * Test that the object is an instance of ActivityProfile
     * @param object
     * @returns {boolean}
     */
    ActivityProfile.is = function (object) {
        return object instanceof ActivityProfile;
    };

    /**
     * Creates an instance of ActivityProfile
     * @param object
     * @returns {ActivityProfile}
     */
    ActivityProfile.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create an activity');
        }
        var args = [];
        activityArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            args.push(object[arg]);
        });
        return ActivityProfile.apply(this, args);
    };

    xapi.ActivityProfile = ActivityProfile;

}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
        typeof require !== 'undefined' ? require : undefined
));
(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        Actor = require ? require('./actor').Actor : xapi.Actor,
        Verb = require ? require('./verb').Verb : xapi.Verb,
        Context = require ? require('./context').Context : xapi.Context,
        Activity = require ? require('./activity').Activity : xapi.Activity,
        StatementRef = require ? require('./statementRef').StatementRef : xapi.StatementRef,
        Attachment = require ? require('./attachment').Attachment : xapi.Attachment,
        Result = require ? require('./result').Result : xapi.Result,

        statementArgs = ['id', 'actor', 'verb', 'object', 'result', 'context', 'timestamp', 'authority', 'attachments', 'stored', 'version'];

    /**
     * Creates an instance of a statement
     * @param id
     * @param actor
     * @param verb
     * @param object
     * @param result
     * @param context
     * @param timestamp
     * @param authority
     * @param attachments
     * @returns {Statement}
     * @constructor
     */
    function Statement(id, actor, verb, object, result, context, timestamp, authority, attachments, stored, version) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Statement.create(arguments[0]);
        }
        if (!(Statement.is(this))) {
            return new Statement(id, actor, verb, object, result, context, timestamp, authority, attachments, stored, version);
        }

        if (id) {
            if (typeof id === 'string' && typeValidator.uuid.test(id)) {
                this.id = id;
            } else {
                throw new Error('Statement invalid id type', errorCode.STATEMENT_INVALID_ID_TYPE);
            }
        }

        if (actor) {
            if (Actor.is(actor)) {
                this.actor = actor;
            } else {
                throw new Error('Statement invalid actor type', errorCode.STATEMENT_INVALID_ACTOR_TYPE);
            }
        } else {
            throw new Error('Statement actor is required', errorCode.MISSING_ACTOR);
        }

        if (verb) {
            if (Verb.is(verb)) {
                this.verb = verb;
            } else {
                throw new Error('Statement invalid verb type', errorCode.STATEMENT_INVALID_VERB_TYPE);
            }
        } else {
            throw new Error('Statement verb is required', errorCode.MISSING_VERB);
        }

        if (object) {
            var objectObjectType = (object.objectType ? object.objectType : '').toLowerCase();
            switch (objectObjectType) {
                case 'substatement':
                    if (Statement.is(object) && typeValidator.substatementType.test(object)) {
                        this.object = object;
                    } else {
                        throw new Error('Statement Object type Sub-Statement is invalid', errorCode.STATEMENT_INVALID_SUB_STATEMENT);
                    }
                    break;
                case 'statementref':
                    if (StatementRef.is(object)) {
                        this.object = object;
                    } else {
                        throw new Error('Statement Object type StatementRef is invalid', errorCode.STATEMENT_INVALID_STATEMENT_REF);
                    }
                    break;
                case 'agent':
                case 'group':
                    if (Actor.is(object)) {
                        this.object = object;
                    } else {
                        throw new Error('Statement Object type agent or group is invalid', errorCode.STATEMENT_INVALID_AGENT_OR_GROUP);
                    }
                    break;
                default:
                    if (Activity.is(object)) {
                        this.object = object;
                    } else {
                        throw new Error('Statement Object type activity is invalid', errorCode.STATEMENT_INVALID_ACTIVITY);
                    }
            }
        } else {
            throw new Error('Statement object is required', errorCode.MISSING_OBJECT);
        }

        if (result) {
            if (Result.is(result)) {
                this.result = result;
            } else {
                throw new Error('Statement invalid result type', errorCode.STATEMENT_INVALID_RESULT_TYPE);
            }
        }

        if (context) {
            if (Context.is(context)) {
                this.context = context;
            } else {
                throw new Error('Statement invalid context type', errorCode.STATEMENT_INVALID_CONTEXT_TYPE);
            }
        }

        if (timestamp) {
            if (timestamp instanceof Date) {
                this.timestamp = timestamp.toISOString();
            } else if (typeof timestamp === 'string' && typeValidator.isoDate.test(timestamp)) {
                this.timestamp = timestamp;
            } else {
                throw new Error('Statement invalid timestamp type', errorCode.STATEMENT_INVALID_TIMESTAMP_TYPE);
            }
        } else {
            this.timestamp = (new Date()).toISOString();
        }

        if (stored) {
            if (stored instanceof Date) {
                this.stored = stored.toISOString();
            } else if (typeof stored === 'string' && typeValidator.isoDate.test(stored)) {
                this.stored = stored;
            } else {
                throw new Error('Statement invalid stored type', errorCode.STATEMENT_INVALID_STORED_TYPE);
            }
        }

        if (authority) {
            if (Actor.is(authority)) {
                this.authority = authority;
            } else {
                throw new Error('Statement invalid authority type', errorCode.STATEMENT_INVALID_AUTHORITY_TYPE);
            }
        }

        if (attachments) {
            if (attachments instanceof Array) {
                attachments.forEach(function (attachment) {
                    if (!Attachment.is(attachment)) {
                        throw new Error('Statement invalid attachment in array', errorCode.STATEMENT_INVALID_ATTACHMENT_ARRAY_TYPE);
                    }
                });
                this.attachments = attachments;
            } else {
                throw new Error('Statement invalid attachments not an array', errorCode.STATEMENT_INVALID_ATTACHMENT_ARRAY_TYPE);
            }
        }

        if (version) {
            this.version = version;
        }
    }

    /**
     * Test that the object is an instance of Statement
     * @param object
     * @returns {boolean}
     */
    Statement.is = function (object) {
        return object instanceof Statement;
    };

    /**
     * Creates an instance of Statement
     * @param object
     * @returns {Statement}
     */
    Statement.create = function (object) {
        if (!object || typeof object !== 'object') {
            throw new Error('object is required to create a statement');
        }
        var args = [];
        statementArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === null)) {
                throw new Error('property ' + arg + ' is null. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                case 'actor':
                    args.push(arg in object ? Actor.create(object[arg]) : undefined);
                    break;
                case 'verb':
                    args.push(arg in object ? Verb.create(object[arg]) : undefined);
                    break;
                case 'object':
                    if (arg in object) {
                        var property = object[arg];
                        var objectType = (property.objectType ? property.objectType : '').toLowerCase();
                        var type = null;
                        switch (objectType) {
                            case 'substatement':
                                type = Statement.create(property);
                                type.objectType = 'SubStatement'; // Must be in this format
                                break;
                            case 'statementref':
                                type = StatementRef.create(property);
                                break;
                            case 'agent':
                            case 'group':
                                type = Actor.create(property);
                                break;
                            default:
                                type = Activity.create(property);
                                break;
                        }
                        args.push(type);
                    }
                    break;
                case 'result':
                    args.push(arg in object ? Result.create(object[arg]) : undefined);
                    break;
                case 'context':
                    args.push(arg in object ? Context.create(object[arg]) : undefined);
                    break;
                case 'authority':
                    args.push(arg in object ? Actor.create(object[arg]) : undefined);
                    break;
                case 'attachments':
                    args.push(arg in object ? Attachment.create(object[arg]) : undefined);
                    break;
                default:
                    args.push(object[arg]);
                    break;
            }
        });
        return Statement.apply(this, args);
    };

    var statementQueryArgs = ['statementId', 'voidedStatementId', 'agent', 'verb', 'activity', 'registration', 'related_activities', 'related_agents', 'since', 'until', 'limit', 'format', 'attachments', 'ascending'];

    // jscs: disable requireCamelCaseOrUpperCaseIdentifiers
    /**
     * StatementQuery constructor
     * @param {string} [statementId]
     * @param {string} [voidedStatementId]
     * @param {object} [agent]
     * @param {IRI} [verb]
     * @param {IRI} [activity]
     * @param {UUID} [registration]
     * @param {boolean} [related_activities=false]
     * @param {boolean} [related_agents=false]
     * @param {Timestamp} [since]
     * @param {Timestamp} [until]
     * @param {number} [limit=0]
     * @param {string} [format=exact] - ids, exact (default), canonical
     * @param {boolean} [attachments=false]
     * @param {boolean} [ascending=false]
     * @returns {State}
     * @constructor
     */
    function StatementQuery(statementId, voidedStatementId, agent, verb, activity, registration, related_activities, related_agents, since, until, limit, format, attachments, ascending) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return StatementQuery.create(arguments[0]);
        }
        if (!(StatementQuery.is(this))) {
            return new StatementQuery(statementId, voidedStatementId, agent, verb, activity, registration, related_activities, related_agents, since, until, limit, format, attachments, ascending);
        }

        if (statementId && voidedStatementId) {
            throw new Error('StatementQuery invalid arguments both statementId and voidedStatementId cannot be used', errorCode.STATEMENT_INVALID_STATEMENT_ID_AND_VOIDED_STATEMENT_ID);
        }

        if (statementId || voidedStatementId) {
            if (agent || verb || activity || registration || related_activities || related_agents || since || until || limit || ascending) {
                throw new Error('StatementQuery invalid arguments when using statementId or voidedStatementId', errorCode.STATEMENT_INVALID_STATEMENT_ID_AND_EXTRA_ARGUMENTS);
            }
        }

        if (statementId) {
            if (typeof statementId === 'string') {
                this.statementId = statementId;
            } else {
                throw new Error('StatementQuery invalid statement id', errorCode.STATEMENT_INVALID_ID_TYPE);
            }
        }

        if (voidedStatementId) {
            if (typeof voidedStatementId === 'string') {
                this.voidedStatementId = voidedStatementId;
            } else {
                throw new Error('StatementQuery invalid voided statement id', errorCode.STATEMENT_INVALID_ID_TYPE);
            }
        }

        if (this.statementId && this.voidedStatementId) {
            throw new Error('StatementQuery cannot request statementId and voidedStatementId', errorCode.STATEMENT_ID_AND_VOIDED_STATEMENT_ID);
        }

        if (agent) {
            if (Actor.is(agent)) {
                if (!agent.objectType || typeValidator.agentOrGroup.test(agent.objectType) || typeValidator.agentOrGroupCase(agent.objectType)) {
                    this.agent = agent;
                } else {
                    throw new Error('StatementQuery invalid agent or group', errorCode.AGENT_INVALID_OBJECT_TYPE);
                }
            } else {
                throw new Error('StatementQuery invalid agent or group', errorCode.AGENT_INVALID_OBJECT_TYPE);
            }
        }

        if (verb) {
            if (typeValidator.uri.test(verb)) {
                this.verb = verb;
            } else {
                throw new Error('StatementQuery invalid verb iri', errorCode.VERB_ID_NOT_IRI);
            }
        }

        if (activity) {
            if (typeValidator.uri.test(activity)) {
                this.activity = activity;
            } else {
                throw new Error('StatementQuery invalid activity iri', errorCode.ACTIVITY_ID_NOT_IRI);
            }
        }

        if (registration) {
            if (typeValidator.uuid.test(registration)) {
                this.registration = registration;
            } else {
                throw new Error('StatementQuery invalid registration type', errorCode.CONTEXT_INVALID_REGISTRATION_TYPE);
            }
        }

        //These default to false according to the spec
        if (related_activities) {
            if (typeValidator.boolean.test(related_activities)) {
                this.related_activities = Boolean(related_activities);
            } else {
                throw new Error('Statement related_activities must be boolean type', errorCode.MUST_BE_BOOLEAN);
            }
        } else {
            this.related_activities = false;
        }
        if (related_agents) {
            if (typeValidator.boolean.test(related_agents)) {
                this.related_agents = Boolean(related_agents);
            } else {
                throw new Error('Statement related_agents must be boolean type', errorCode.MUST_BE_BOOLEAN);
            }
        } else {
            this.related_agents = false;
        }

        // Default to 0 epoch, if no statementId or voidedStatementId in query
        if (since) {
            if (since instanceof Date) {
                this.since = since.toISOString();
            } else if (typeof since === 'string' && typeValidator.isoDate.test(since)) {
                this.since = since;
            } else {
                throw new Error('StatementQuery invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        } else if (!statementId && !voidedStatementId) {
            // Only add the since time field if statementId or voidedStatementId is not present
            this.since = new Date(0).toISOString();
        }

        if (until) {
            if (until instanceof Date) {
                this.until = until.toISOString();
            } else if (typeof until === 'string' && typeValidator.isoDate.test(until)) {
                this.until = until;
            } else {
                throw new Error('StatementQuery invalid timestamp type', errorCode.TIMESTAMP_INVALID_ISO8601_FORMAT);
            }
        }

        //defaults to 0
        if (limit) {
            if (isFinite(limit) && Number(limit) > 0) {
                this.limit = Number(limit);
            } else {
                throw new Error('StatementQuery limit must be positive number or 0', errorCode.LIMIT_INVALID_NUMBER);
            }
        } else {
            this.limit = 0;
        }

        //defaults to exact
        if (format) {
            if (['ids', 'exact', 'canonical'].indexOf(format) > -1) {
                this.format = format;
            } else {
                throw new Error('StatementQuery format must be ids, exact, or canonical', errorCode.STATEMENT_INVALID_FORMAT_TYPE);
            }
        } else {
            this.format = 'exact';
        }

        //These default to false according to the spec
        if (attachments) {
            if (typeValidator.boolean.test(attachments)) {
                this.attachments = Boolean(attachments);
            } else {
                throw new Error('Statement attachments must be boolean type', errorCode.MUST_BE_BOOLEAN);
            }
        } else {
            this.attachments = false;
        }
        if (ascending) {
            if (typeValidator.boolean.test(ascending)) {
                this.ascending = Boolean(ascending);
            } else {
                throw new Error('Statement ascending must be boolean type', errorCode.MUST_BE_BOOLEAN);
            }
        } else {
            this.ascending = false;
        }
    }

    // jscs: enable requireCamelCaseOrUpperCaseIdentifiers

    /**
     * Test that the object is an instance of StatementQuery
     * @param object
     * @returns {boolean}
     */
    StatementQuery.is = function (object) {
        return object instanceof StatementQuery;
    };

    /**
     * Creates an instance of StatementQuery
     * @param object
     * @returns {StatementQuery}
     */
    StatementQuery.create = function (object) {
        var args = [];

        statementQueryArgs.forEach(function (arg) {
            if (arg in object && (object[arg] === undefined || object[arg] === null)) {
                throw new Error('property ' + arg + ' is null or undefined. properties must contain a value', errorCode.XAPI_PROPERTY_VIOLATION);
            }

            switch (arg) {
                case 'agent':
                    args.push(arg in object ? Actor.create(object[arg]) : undefined);
                    break;
                default:
                    args.push(object[arg]);
            }
        });
        return StatementQuery.apply(this, args);
    };

    var statementsArgs = ['statements'];

    function Statements(statements) {
        if (!(Statements.is(this))) {
            return new Statements(statements);
        }
        this.statements = [];
        if (statements) {
            if ((typeof statements) === 'object') {
                if (statements instanceof Array && statements.length > 0) {
                    statements.forEach(function (statement) {
                        this.add(statement);
                    }.bind(this));
                }
            } else {
                throw new Error('Statements invalid argument must be Object or Array but got type:' + typeof (statements));
            }
        }
    }

    /**
     * Adds a Statement to a Statements array
     * @param {Statement} statement
     */
    Statements.prototype.add = function (statement) {
        this.statements.push(Statement.is(statement) ? statement : Statement.create(statement));
    };

    /**
     * Test that the object is an instance of Statements
     * @param object
     * @returns {boolean}
     */
    Statements.is = function (object) {
        return object instanceof Statements;
    };

    Statements.create = function (object) {
        var argObject = object || {},
            args = [];
        statementsArgs.forEach(function (arg) {
            args.push(arg in argObject ? argObject[arg] : undefined);
        });
        return Statements.apply(this, args);
    };

    Statement.prototype.create = function(object){
        return Statement.create(Object.create(this, object));
    }


    xapi.Statements = Statements;
    xapi.Statement = Statement;
    xapi.StatementQuery = StatementQuery;
}(
    (typeof module !== 'undefined' && module.exports) ? exports : ('xapi' in this ? this.xapi : this.xapi = {}),
    (typeof module !== 'undefined' && module.exports) ? require('./../validators/typeValidator') : (this.typeValidator),
    typeof require !== 'undefined' ? require : undefined
));
(function (xapi, request) {
    'use strict';

    /**
     * About constructor
     * @returns {About}
     * @constructor
     */
    function About() {

    }

    /**
     * Returns JSON Object containing information about this LRS, including the xAPI version supported.
     * @param {function} cb - callback(err, response, statusText, xhr)
     * @returns {object}
     */
    About.get = function (cb) {
        request.ajax({
            url: xapi.http + '/about',
            type: 'GET',
            success: function (data, statusText, xhr) {
                cb(null, data, statusText, xhr);
            },
            error: function (xhr, statusText, err) {
                cb(err, null, statusText, xhr);
            }
        });
    };

    xapi.About = About;

}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.request
));
(function (xapi, typeValidator, request) {
    'use strict';

    var ActivityProfile = xapi.ActivityProfile,
        Activity = xapi.Activity,
        ActivityDefinition = xapi.ActivityDefinition;

    /**
     * Loads the complete ActivityProfile Object specified.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    Activity.prototype.get = function (cb) {
        request.ajax({
            url: xapi.http + '/activities',
            type: 'GET',
            params: this,
            success: function (data, statusText, xhr) {
                if (data.definition) {
                    try {
                        this.setDefinition(ActivityDefinition.create(data.definition));
                    } catch (e) {
                        return cb(e, data, statusText, xhr);
                    }
                }
                cb(null, data, statusText, xhr);
            }.bind(this),
            error: function (xhr, statusText, err) {
                cb(err, null, statusText, xhr);
            }
        });
    };

    /**
     * Loads ids of all profile entries for an ActivityProfile. If 'since' parameter is specified, this is limited to entries that have been stored or updated since the specified timestamp (exclusive).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    ActivityProfile.prototype.getAll = function (cb) {
        if (!this.profileId) {
            request.ajax({
                url: xapi.http + '/activities/profile',
                type: 'GET',
                params: this,
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id cannot be used in ActivityProfile for getAll', xapi.errorCode.PROFILE_ID_INVALID_TYPE);
        }
    };

    /**
     * Retrieves the specified profile document in the context of the specified ActivityProfile.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    ActivityProfile.prototype.get = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/activities/profile',
                type: 'GET',
                params: this,
                success: function (data, statusText, xhr) {
                    var newDocument = new xapi.Document({
                        contents: data
                    });
                    this.setDocument(newDocument);
                    cb(null, data, statusText, xhr);
                }.bind(this),
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for ActivityProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Updates the specified profile document in the context of the specified ActivityProfile.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    ActivityProfile.prototype.update = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/activities/profile',
                type: 'PUT',
                params: this,
                data: this.getDocument().contents,
                headers: {id: this.getDocument().id, updated: this.getDocument().updated, "if-match": '*'},
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for ActivityProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Creates the specified profile document in the context of the specified ActivityDefinition.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    ActivityProfile.prototype.post = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/activities/profile',
                type: 'POST',
                params: this,
                data: this.getDocument().contents,
                headers: {id: this.getDocument().id, updated: this.getDocument().updated},
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for ActivityProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Deletes the specified profile document in the context of the specified ActivityDefinition.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    ActivityProfile.prototype.remove = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/activities/profile',
                type: 'DELETE',
                params: this,
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for ActivityProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.typeValidator,
    this.request
));
(function (xapi, typeValidator, request) {
    'use strict';

    var AgentProfile = xapi.AgentProfile,
        Actor = xapi.Actor;

    Actor.prototype.get = function (cb) {
        if (this.isAgent()) {
            request.ajax({
                url: xapi.http + '/agents',
                type: 'GET',
                params: this,
                success: function (data, statusText, xhr) {
                    try {
                        return cb(null, data, statusText, xhr);
                    } catch (e) {
                        cb(e, data, statusText, xhr);
                    }
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('Actor can only get Agents object type is not agent', xapi.errorCode.AGENT_INVALID_OBJECT_TYPE);
        }
    };

    /**
     * Loads ids of all profile entries for an Agent. If 'since' parameter is specified, this is limited to entries that have been stored or updated since the specified timestamp (exclusive).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    AgentProfile.prototype.getAll = function (cb) {
        if (!this.profileId) {
            request.ajax({
                url: xapi.http + '/agents/profile',
                type: 'GET',
                params: this,
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id cannot be used in AgentProfile for getAll', xapi.errorCode.PROFILE_ID_INVALID_TYPE);
        }
    };

    /**
     * Retrieves the specified profile document in the context of the specified Agent.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    AgentProfile.prototype.get = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/agents/profile',
                type: 'GET',
                params: this,
                success: function (data, statusText, xhr) {
                    var newDocument = new xapi.Document({
                        contents: data
                    });
                    this.setDocument(newDocument);
                    cb(null, data, statusText, xhr);
                }.bind(this),
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for AgentProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Updates the specified profile document in the context of the specified Agent.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    AgentProfile.prototype.update = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/agents/profile',
                type: 'PUT',
                params: this,
                data: this.getDocument().contents,
                headers: {id: this.getDocument().id, updated: this.getDocument().updated, "if-match": '*'},
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for AgentProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Creates the specified profile document in the context of the specified Agent.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    AgentProfile.prototype.post = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/agents/profile',
                type: 'POST',
                params: this,
                data: this.getDocument().contents,
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for AgentProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

    /**
     * Deletes the specified profile document in the context of the specified Agent.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    AgentProfile.prototype.remove = function (cb) {
        if (this.profileId) {
            request.ajax({
                url: xapi.http + '/agents/profile',
                type: 'DELETE',
                params: this,
                success: function (data, statusText, xhr) {
                    cb(null, data, statusText, xhr);
                },
                error: function (xhr, statusText, err) {
                    cb(err, null, statusText, xhr);
                }
            });
        } else {
            throw new Error('profile id required for AgentProfile', xapi.errorCode.PROFILE_ID_REQUIRED);
        }
    };

}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.typeValidator,
    this.request
));
/**
 * Description: Generic structure to store items that might not have been saved. Loads items from local storage, then session storage.
 */
(function (xapi, request) {
    'use strict';

    /**
     * Backlog constructor
     * @param name to be used for local and session storage.
     * @constructor
     */
    function Backlog(name) {

        this.name = name; // Name for storage

        this.backlog = [];

        // Restore data from storage
        this.get();
    }

    /**
     * Adds a new item to backlog.
     * @param statements to be added.
     */
    Backlog.prototype.add = function (statements) {

        if (Array.isArray(statements)) {
            this.backlog = this.backlog.concat(statements);
        } else {
            this.backlog.push(statements);
        }

        this.syncStorage(this.backlog);
    };

    /**
     * Gets the current array of backlog items.
     * @returns {*}
     */
    Backlog.prototype.get = function () {
        var value;
        if (sessionStorage) {
            value = sessionStorage.getItem(this.name);
            if (value) {
                try {
                    this.backlog = JSON.parse(value);
                } catch (err) {
                    console.log(err);
                }
            }
        }

        if (localStorage) {
            value = localStorage.getItem(this.name);
            if (value) {
                try {
                    this.backlog = JSON.parse(value);
                } catch (err) {
                    console.log(err);
                }
            }
        }

        return this.backlog;
    };

    /**
     * Returns the length of the backlog.
     * @returns {*}
     */
    Backlog.prototype.getLength = function () {
        return this.get().length;
    };

    /**
     * Syncs the current backlog to session and local storage.
     * @param statements
     */
    Backlog.prototype.syncStorage = function (statements) {
        if (localStorage) {
            localStorage.setItem(this.name, JSON.stringify(statements));
        }

        if (sessionStorage) {
            sessionStorage.setItem(this.name, JSON.stringify(statements));
        }
    };

    /**
     * Clear the current backlog.
     */
    Backlog.prototype.empty = function () {
        this.backlog = [];
        this.syncStorage([]);
    };

    /**
     * Attempts to send the messages in the backlog
     *
     * @param cb
     */
    Backlog.prototype.send = function send(cb) {
        var self = this;
        var statements = self.get();
        request.ajax({
            url: xapi.http + '/statements',
            type: 'POST',
            data: statements,
            success: function (data, statusText, xhr) {
                self.backlog.splice(statements.length);
                self.syncStorage(self.backlog);
                cb(null, data, statusText, xhr);
            },
            error: function (xhr, statusText, err) {
                // This means that either the client has lost connection or the LRS is down. Backlog the item.
                cb(err, null, statusText, xhr);
            }
        });
    };

    xapi.Backlog = Backlog;

}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.request
));

(function (xapi, typeValidator, request) {
    'use strict';

    var State = xapi.State;

    /**
     * Fetches the document specified by the given stateId that exists in the context of the specified ActivityDefinition, Agent, and registration (if specified).
     * @@param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.get = function (cb) {
        if (this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'GET',
                             params: this,
                             success: function (data, statusText, xhr) {
                                 var newDocument = new xapi.Document({
                                     contents: data
                                 });
                                 this.setDocument(newDocument);
                                 cb(null, data, statusText, xhr);
                             }.bind(this),
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id is required for single document requests', xapi.errorCode.STATE_ID_REQUIRED);
        }
    };

    /**
     * Creates the document specified by the given stateId that exists in the context of the specified ActivityDefinition, Agent, and registration (if specified).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.post = function (cb) {
        if (this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'POST',
                             params: this,
                             data: this.getDocument().contents,
                             headers: {id: this.getDocument().id, updated: this.getDocument().updated},
                             success: function (data, statusText, xhr) {
                                 cb(null, data, statusText, xhr);
                             },
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id is required for single document requests', xapi.errorCode.STATE_ID_REQUIRED);
        }
    };

    /**
     * Updates the document specified by the given stateId that exists in the context of the specified ActivityDefinition, Agent, and registration (if specified).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.update = function (cb) {
        if (this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'PUT',
                             params: this,
                             data: this.getDocument().contents,
                             headers: {id: this.getDocument().id, updated: this.getDocument().updated},
                             success: function (data, statusText, xhr) {
                                 cb(null, data, statusText, xhr);
                             },
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id is required for single document requests', xapi.errorCode.STATE_ID_REQUIRED);
        }
    };

    /**
     * Deletes the document specified by the given stateId that exists in the context of the specified ActivityDefinition, Agent, and registration (if specified).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.remove = function (cb) {
        if (this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'DELETE',
                             params: this,
                             headers: {id: this.getDocument().id, updated: this.getDocument().updated},
                             success: function (data, statusText, xhr) {
                                 cb(null, data, statusText, xhr);
                             },
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id is required for single document requests', xapi.errorCode.STATE_ID_REQUIRED);
        }
    };

    /**
     * Fetches ids of all state data for this context (ActivityDefinition + Agent [ + registration if specified]). If "since" parameter is specified, this is limited to entries that have been stored or updated since the specified timestamp (exclusive).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.getAll = function (cb) {
        if (!this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'GET',
                             params: this,
                             success: function (data, statusText, xhr) {
                                 cb(null, data, statusText, xhr);
                             },
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id cannot be used for getAll request', xapi.errorCode.STATE_ID_INVALID);
        }
    };

    /**
     * Deletes all state data for this context (ActivityDefinition + Agent [+ registration if specified]).
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    State.prototype.removeAll = function (cb) {
        if (!this.stateId) {
            request.ajax({
                             url: xapi.http + '/activities/state',
                             type: 'DELETE',
                             params: this,
                             success: function (data, statusText, xhr) {
                                 cb(null, data, statusText, xhr);
                             },
                             error: function (xhr, statusText, err) {
                                 cb(err, null, statusText, xhr);
                             }
                         });
        } else {
            throw new Error('state id cannot be used for removeAll request', xapi.errorCode.STATE_ID_INVALID);
        }
    };

}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.typeValidator,
    this.request
));
(function (xapi, typeValidator, request, io) {
    'use strict';

    var Statement = xapi.Statement,
        Statements = xapi.Statements,
        StatementQuery = xapi.StatementQuery,
        backlog = new xapi.Backlog('statements_backlog');

    xapi.StatementsBacklog = backlog;

    /**
     * Stores Statement with the given id.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    Statement.prototype.store = function (cb) {

        var statements,
            fillingBacklog = false,
            files = [];
        if (backlog.getLength() > 0) {
            backlog.add(this);
            statements = backlog.get();
            fillingBacklog = true;
        }

        if (this.files) {
            files = this.files;
            delete this.files;
        }

        request.ajax({
            url: xapi.http + '/statements',
            type: fillingBacklog ? 'POST' : 'PUT',
            data: fillingBacklog ? statements : this,
            files: files,
            success: function (data, statusText, xhr) {
                delete this;

                if (fillingBacklog) {
                    backlog.empty();
                }

                cb(null, data, statusText, xhr);
            }.bind(this),
            error: function (xhr, statusText, err) {
                // This means that either the client has lost connection or the LRS is down. Backlog the item.
                if (!fillingBacklog && xhr.status === 0) {
                    backlog.add(this);
                    cb(err, null, statusText, xhr, true);
                } else {
                    cb(err, null, statusText, xhr);
                }

            }.bind(this)
        });
    };

    /**
     * Test that the object is an instance of Statements
     * @param object
     * @returns {boolean}
     */

    /**
     * Stores a set of Statements. Since the Statement.store method targets a specific Statement id, Statements.store must be used rather than Statement.store to save multiple Statements, or to save one Statement without first generating a Statement id.
     * @param {function} cb - callback(err, response, statusText, xhr)
     */
    Statements.prototype.store = function (cb) {

        /**
         * If a backlog exists, add statements to backlog and try to complete request, else just
         * complete it like normal
         */
        var statements,
            fillingBacklog = false,
            files = [];
        if (backlog.getLength() > 0) {
            backlog.add(this.statements);
            this.statements = [];
            statements = backlog.get();
            fillingBacklog = true;
        } else {
            statements = this.statements;
        }

        for (var i = 0; i < statements.length; i++) {
            if (statements[i].files) {
                files = files.concat(statements[i].files);
                delete statements[i].files;
            }
        }

        request.ajax({
            url: xapi.http + '/statements',
            type: 'POST',
            data: statements,
            files: files,
            success: function (data, statusText, xhr) {
                statements = [];
                this.statements = this.statements.splice(data.length);

                if (fillingBacklog) {
                    backlog.empty();
                }

                cb(null, data, statusText, xhr);
            }.bind(this),
            error: function (xhr, statusText, err) {

                // This means that either the client has lost connection or the LRS is down. Backlog the item.
                if (!fillingBacklog && xhr.status === 0) {
                    backlog.add(statements);
                    cb(err, null, statusText, xhr, true);
                } else {
                    cb(err, null, statusText, xhr);
                }
            }.bind(this)
        });
    };

    /**
     * This method may be called to fetch a single Statement or multiple Statements. If the statementId or voidedStatementId parameter is specified a single Statement is returned.
     * @param {StatementQuery~getCallback} cb - callback(err, response, statusText, xhr)
     *
     */
    StatementQuery.prototype.get = function (cb) {
        request.ajax({
            url: xapi.http + '/statements',
            type: 'GET',
            params: this,
            contentType: this.attachments ? 'multipart/mixed' : undefined,
            success: function (data, statusText, xhr) {
                cb(null, data, statusText, xhr);
            },
            error: function (xhr, statusText, err) {
                cb(err, null, statusText, xhr);
            }
        });
    };


    /**
     *
     * @param {object} options - options for live query
     * @param {function} options.update - a callback where updated are pushed into
     * @param {function} [options.connect] - callback when a connection is established
     * @param {function} [options.disconnect] - called when we disconnect
     * @param {function} [options.error] - callback for socket.io error and matching error
     * @param {function} [options.data] - data to seed based on the query object
     */
    var liveOptions = ['update', 'connect', 'disconnect', 'error', 'data'];
    StatementQuery.prototype.live = function (options) {
        var client = io(xapi.ws + '/statements');
        if (options.update) {
            client.on('match', options.update);
        } else {
            throw new Error("StatementQuery.live requires an options.update");
        }
        liveOptions.forEach(function (key) {
            if (typeof options[key] !== 'function') {
                throw new Error("StatementQuery.live option['" + key + "'] must be a function");
            }
        });
        if (options.connect) {
            client.on('connect', options.connect);
        }
        if (options.error) {
            client.on('error', options.error);
            client.on('liveError', options.error);
        }
        if (options.disconnect) {
            client.on('disconnect', options.disconnect);
        }
        client.emit('register', this, options.data);
    };


}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.typeValidator,
    this.request,
    this.io
));
(function (xapi) {
    'use strict';
    var base = 'localhost.com/lrs',
        client = 'www';
    xapi.version = '1.0.2';
    xapi.http = '//' +  client + '.' + base;
    xapi.ws = '//' +  client + '.' + base + '/' + client;
    xapi.setLRS = function setLRS(http){
        xapi.http = http;
    };
}(
    ('xapi' in this ? this.xapi : this.xapi = {})
));
