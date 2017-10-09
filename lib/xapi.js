(function(window, define) {/**
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
        SINCE_INVALID: 150,
        AUTHORITY_MUST_NOT_INCLUDE_FUNCTIONAL_IDENTIFIERS: 151,
        INVALID_STATEMENTREF_PROPERTIES: 158,
        INVALID_CONTEXT_STATEMENT_OBJECTTYPE: 157,
        OBJECT_INVALID_INTERACTION_TYPE: 152,
        OBJECT_INVALID_INTERACTION_COMPONENT_TYPE: 153,
        ACTIVITY_DEFINITION_MUST_BE_OBJECT: 154,
        UNEXPECTED_STATEMENT_PROPERTY: 156,
        INVALID_ACTOR_OBJECT_TYPE: 155,
        MISSING_INTERACTION_TYPE: 159,
        DOCUMENT_CONTENT_TYPE_REQUIRED: 160

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
        if (XMLHttpRequest) {
            // Per the spec `xhr.withCredentials` must be set AFTER the request is opened
            // See https://xhr.spec.whatwg.org/#the-withcredentials-attribute
            req.withCredentials = true;
        }

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
        '(?:(?:[A-Za-z][A-Za-z0-9+\-.]*)://)' +
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
    exports.interactionType = RegExp('^(choice|sequencing|likert|matching|performance|true-false|long-fill-in|fill-in|numeric|other)$');
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

/*global window, require, define */
(function(_window) {
  'use strict';

  // Unique ID creation requires a high quality random # generator.  We feature
  // detect to determine the best RNG source, normalizing to a function that
  // returns 128-bits of randomness, since that's what's usually required
  var _rng, _mathRNG, _nodeRNG, _whatwgRNG, _previousRoot;

  function setupBrowser() {
    // Allow for MSIE11 msCrypto
    var _crypto = _window.crypto || _window.msCrypto;

    if (!_rng && _crypto && _crypto.getRandomValues) {
      // WHATWG crypto-based RNG - http://wiki.whatwg.org/wiki/Crypto
      //
      // Moderately fast, high quality
      try {
        var _rnds8 = new Uint8Array(16);
        _whatwgRNG = _rng = function whatwgRNG() {
          _crypto.getRandomValues(_rnds8);
          return _rnds8;
        };
        _rng();
      } catch(e) {}
    }

    if (!_rng) {
      // Math.random()-based (RNG)
      //
      // If all else fails, use Math.random().  It's fast, but is of unspecified
      // quality.
      var  _rnds = new Array(16);
      _mathRNG = _rng = function() {
        for (var i = 0, r; i < 16; i++) {
          if ((i & 0x03) === 0) { r = Math.random() * 0x100000000; }
          _rnds[i] = r >>> ((i & 0x03) << 3) & 0xff;
        }

        return _rnds;
      };
      if ('undefined' !== typeof console && console.warn) {
        console.warn("[SECURITY] node-uuid: crypto not usable, falling back to insecure Math.random()");
      }
    }
  }

  function setupNode() {
    // Node.js crypto-based RNG - http://nodejs.org/docs/v0.6.2/api/crypto.html
    //
    // Moderately fast, high quality
    if ('function' === typeof require) {
      try {
        var _rb = require('crypto').randomBytes;
        _nodeRNG = _rng = _rb && function() {return _rb(16);};
        _rng();
      } catch(e) {}
    }
  }

  if (_window) {
    setupBrowser();
  } else {
    setupNode();
  }

  // Buffer class to use
  var BufferClass = ('function' === typeof Buffer) ? Buffer : Array;

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

    var clockseq = (options.clockseq != null) ? options.clockseq : _clockseq;

    // UUID timestamps are 100 nano-second units since the Gregorian epoch,
    // (1582-10-15 00:00).  JSNumbers aren't precise enough for this, so
    // time is handled internally as 'msecs' (integer milliseconds) and 'nsecs'
    // (100-nanoseconds offset from msecs) since unix epoch, 1970-01-01 00:00.
    var msecs = (options.msecs != null) ? options.msecs : new Date().getTime();

    // Per 4.2.1.2, use count of uuid's generated during the current clock
    // cycle to simulate higher resolution clock
    var nsecs = (options.nsecs != null) ? options.nsecs : _lastNSecs + 1;

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

    if (typeof(options) === 'string') {
      buf = (options === 'binary') ? new BufferClass(16) : null;
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
  uuid._rng = _rng;
  uuid._mathRNG = _mathRNG;
  uuid._nodeRNG = _nodeRNG;
  uuid._whatwgRNG = _whatwgRNG;

  if (('undefined' !== typeof module) && module.exports) {
    // Publish as node.js module
    module.exports = uuid;
  } else if (typeof define === 'function' && define.amd) {
    // Publish as AMD module
    define(function() {return uuid;});


  } else {
    // Publish as global (in browsers)
    _previousRoot = _window.uuid;

    // **`noConflict()` - (browser only) to reset global 'uuid' var**
    uuid.noConflict = function() {
      _window.uuid = _previousRoot;
      return uuid;
    };

    _window.uuid = uuid;
  }
})('undefined' !== typeof window ? window : null);
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
	    /*
	     * Local polyfil of Object.create
	     */
	    var create = Object.create || (function () {
	        function F() {};

	        return function (obj) {
	            var subtype;

	            F.prototype = obj;

	            subtype = new F();

	            F.prototype = null;

	            return subtype;
	        };
	    }())

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
	                var subtype = create(this);

	                // Augment
	                if (overrides) {
	                    subtype.mixIn(overrides);
	                }

	                // Create default initializer
	                if (!subtype.hasOwnProperty('init') || this.init === subtype.init) {
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
	            } else {
	                // Copy one word at a time
	                for (var i = 0; i < thatSigBytes; i += 4) {
	                    thisWords[(thisSigBytes + i) >>> 2] = thatWords[i >>> 2];
	                }
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
));

(function (xapi, typeValidator, require) {
    'use strict';
    var errorCode = require ? require('./../errors').errorCode : xapi.errorCode,
        Error = require ? require('./../errors').Error : xapi.Error,
        documentArgs = ['id', 'updated', 'contents', 'contentType'];

    /**
     * Document constructor
     * @param {String} id
     * @param {Timestamp} updated
     * @param {String} contents
     * @param {String} contentType
     * @returns {Document}
     * @constructor
     */
    function Document(id, updated, contents, contentType) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return Document.create(arguments[0]);
        }
        if (!(Document.is(this))) {
            return new Document(id, updated, contents, contentType);
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

        if (contentType) {
            this.contentType = contentType;
            this.isMergeable = contentType === 'application/json';
        } else {
            throw new Error('document content type required', errorCode.DOCUMENT_CONTENT_TYPE_REQUIRED);
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
        if (score !== undefined) {
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

        if (response !== undefined) {
            if (typeof response === 'string') {
                this.response = response;
            } else {
                throw new Error('Result invalid response type : ' + response, errorCode.RESULTS_INVALID_RESPONSE);
            }
        }

        if (duration !== undefined) {
            if (duration instanceof Date) {
                this.duration = duration.toISOString();
            } else if (typeof duration === 'string' && typeValidator.isoDuration.test(duration)) {
                this.duration = duration;
            } else {
                throw new Error('Result invalid duration type : ' + duration, errorCode.RESULTS_INVALID_DURATION);
            }
        }

        if (extensions !== undefined) {
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
        (typeof module !== 'undefined' && module.exports) ? require : undefined
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
        (typeof module !== 'undefined' && module.exports) ? require : undefined
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
        (typeof module !== 'undefined' && module.exports) ? require : undefined
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
                this.platform = platform;
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
        (typeof module !== 'undefined' && module.exports) ? require : undefined
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
        (typeof module !== 'undefined' && module.exports) ? require : undefined
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

    var statementQueryArgs = ['statementId', 'voidedStatementId', 'agent', 'verb', 'activity', 'registration', 'related_activities', 'related_agents', 'since', 'until', 'limit', 'format', 'attachments', 'ascending', 'sortColumn'];

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
     * @param {string} [sortColumn]
     * @returns {State}
     * @constructor
     */
    function StatementQuery(statementId, voidedStatementId, agent, verb, activity, registration, related_activities, related_agents, since, until, limit, format, attachments, ascending, sortColumn) {
        if (arguments.length === 1 && typeof arguments[0] === 'object') {
            return StatementQuery.create(arguments[0]);
        }
        if (!(StatementQuery.is(this))) {
            return new StatementQuery(statementId, voidedStatementId, agent, verb, activity, registration, related_activities, related_agents, since, until, limit, format, attachments, ascending, sortColumn);
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
        if (sortColumn) {
            this.sortColumn = sortColumn;
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
    (typeof module !== 'undefined' && module.exports) ? require : undefined
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
            type: 'POST',
            data: fillingBacklog ? statements : this,
            files: files,
            success: function (data, statusText, xhr) {

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
}(
    ('xapi' in this ? this.xapi : this.xapi = {}),
    this.typeValidator,
    this.request
));
(function (xapi) {
    'use strict';
    var base = 'test.com',
        client = 'www';
    xapi.version = '1.0.3';
    xapi.http = '//' +  client + '.' + base;
    xapi.ws = '//' +  client + '.' + base + '/' + client;
    xapi.setLRS = function setLRS(http){
        xapi.http = http;
    };
}(
    ('xapi' in this ? this.xapi : this.xapi = {})
));
}(window, undefined));